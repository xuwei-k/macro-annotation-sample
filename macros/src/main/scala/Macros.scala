package actormacro

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox._
import language.experimental.macros

class create extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro create.impl
}

object create {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val inputs : List[Tree] = annottees.map(_.tree)(collection.breakOut)
    val outputs: List[Tree] = inputs match {
      case (classDef @ ClassDef(_, cName @ TypeName(className), types, templates)) :: tail =>
        val cc = c.mirror.staticClass("scalaz." + className.replace("Ops", "")).toType
        val methods = cc.decls.collect{case m: MethodSymbol => m}
        val F = cc.typeParams.head.typeSignature.erasure
        val hasF = methods.filter(_.typeParams.nonEmpty).map{ method =>
          method -> method.paramLists.map{ params =>
            params.map{ param =>
              val t = param.typeSignatureIn(cc)
              PartialFunction.condOpt(t.typeArgs){
                case arg :: Nil =>
                  try{
                    cc.typeParams.head.asType.toType.typeConstructor =:= t.typeConstructor
                  }catch{
                    case _: NoSuchElementException => false
                  }
              }.getOrElse(false)
            }.zipWithIndex.filter(_._1).map(_._2)
          }.zipWithIndex.collect{
            case (x, i) if x.nonEmpty => x.map(i -> _)
          }.flatten
        }.filter(_._2.nonEmpty)

        if(hasF.forall(_._2.size != 1)){
          println(hasF.groupBy(_._2.size).map{case (k, v) => k -> v.size})
        }else{
          println(hasF.groupBy(_._2).map{case (k, v) => k -> v.size})
        }

        println(hasF.size)

        val newMethods = hasF.map{ case (method, index :: Nil) =>
          val newParams = method.paramLists.zipWithIndex.map{
            case (params, j1) =>
              params.zipWithIndex.collect{
                case (param, j2) if index != (j1, j2) => param
              }
          }.filter(_.nonEmpty)

          def symbol2typeDef(s: Symbol): TypeDef =
            TypeDef(
              NoMods,
              s.name.toTypeName,
              s.asType.typeParams.map(s => symbol2typeDef(s)),
              EmptyTree
            )

          def symbol2valdef(s: Symbol): ValDef = ValDef(
            Modifiers(Flag.PARAM),
            s.name.toTermName,
            TypeTree(s.typeSignature),
            EmptyTree
          )

          DefDef(
            NoMods,
            method.name,
            method.typeParams.map(t => symbol2typeDef(t)).filter(_.name.toString != "A"), // TODO
            newParams.map(_.map(s => symbol2valdef(s))),
            TypeTree(method.returnType),
            EmptyTree
          )
        }

        val createMethod = q""

        println(newMethods.map(t => showCode(t)).mkString("\n"))

        val mod0: ModuleDef = tail match {
          case (md @ ModuleDef(_, mName, mTemp)) :: Nil 
            if cName.decodedName.toString == mName.decodedName.toString => md

          case Nil =>
            val cMod  = classDef.mods
            var mModF = NoFlags
            if (cMod hasFlag Flag.PRIVATE  ) mModF |= Flag.PRIVATE
            if (cMod hasFlag Flag.PROTECTED) mModF |= Flag.PROTECTED
            if (cMod hasFlag Flag.LOCAL    ) mModF |= Flag.LOCAL
            val mMod = Modifiers(mModF, cMod.privateWithin, Nil)

            val mkSuperSelect = Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), 
                                       termNames.CONSTRUCTOR)
            val superCall     = Apply(mkSuperSelect, Nil)
            val constr        = DefDef(NoMods, termNames.CONSTRUCTOR, Nil, List(Nil), 
              TypeTree(), Block(List(superCall), Literal(Constant(()))))

            val mTemp = Template(parents = List(TypeTree(typeOf[AnyRef])), 
              self = noSelfType, body = constr :: Nil)
            val mName = TermName(cName.decodedName.toString)

            ModuleDef(mMod, mName, mTemp)

          case _ => c.abort(c.enclosingPosition, "Expected a companion object")
        }

        val Template(mTempParents, mTempSelf, mTempBody0) = mod0.impl

        val mTempBody1  = createMethod :: mTempBody0
        val mTemp1      = Template(mTempParents, mTempSelf, mTempBody1)
        val mod1        = ModuleDef(mod0.mods, mod0.name, mTemp1)

        val newClassDef = ClassDef(
          classDef.mods, classDef.name, classDef.tparams,
          Template(
            classDef.impl.parents,
            classDef.impl.self,
            classDef.impl.body ::: newMethods.toList
          )
        )

        newClassDef :: mod1 :: Nil

      case other =>
        c.abort(c.enclosingPosition, "Must annotate a class or trait")
    }

    val expr = c.Expr[Any](Block(outputs, Literal(Constant(()))))
    println(expr.tree)
    expr
  }
}

