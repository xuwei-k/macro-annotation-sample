package macroz

import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

class create extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro Impl.impl
}

final class Impl(val c: Context) {
  import c.universe._

  private def symbol2typeDef(s: Symbol): TypeDef = TypeDef(
    Modifiers(Flag.PARAM),
    s.name.toTypeName,
    s.asType.typeParams.map(s => symbol2typeDef(s)),
    TypeBoundsTree(TypeTree(), TypeTree())
  )

  private def symbol2valdef(s: Symbol): ValDef = {
    val flags = if(s.isImplicit) Flag.PARAM | Flag.IMPLICIT else Flag.PARAM
    ValDef(
      Modifiers(flags),
      s.name.toTermName,
      TypeTree(s.typeSignature),
      EmptyTree
    )
  }

  def impl(annottees: c.Tree*): c.Expr[Any] = {

    val inputs : List[Tree] = annottees.toList
    val outputs: List[Tree] = inputs match {
      case (classDef @ ClassDef(_, cName @ TypeName(className), types, templates)) :: tail =>
        val typeClassName = "scalaz." + className.replace("Ops", "")
        val cc = c.mirror.staticClass(typeClassName).toType
        val methods = cc.decls.collect{case m: MethodSymbol => m}
        val F = cc.typeParams.head.typeSignature.erasure
        val hasF = methods.filter(_.typeParams.nonEmpty).map{ method =>
          method -> method.paramLists.map{ params =>
            params.map{ param =>
              val t = param.typeSignatureIn(cc)
              PartialFunction.condOpt(t.typeArgs){
                case arg :: Nil =>
                  try{
                    val isA = arg.toString == "A"
                    val const = cc.typeParams.head.asType.toType.typeConstructor =:= t.typeConstructor
                    isA && const && (!arg.typeConstructor.takesTypeArgs)
                  }catch{
                    case _: NoSuchElementException => false
                  }
              }.getOrElse(false)
            }.zipWithIndex.filter(_._1).map(_._2)
          }.zipWithIndex.collect{
            case (x, i) if x.nonEmpty => x.map(i -> _)
          }.flatten
        }.filter(_._2.nonEmpty)

        println(hasF.size)

        val newMethods = hasF.map{ case (method, index :: Nil) =>
          val newParams = method.paramLists.zipWithIndex.map{
            case (params, j1) =>
              params.zipWithIndex.collect{
                case (param, j2) if index != (j1, j2) => param
              }
          }.filter(_.nonEmpty)

          val body = method.paramLists.zipWithIndex.foldLeft(
            q"this.F.${method.name}" : Tree
          ) {
            case (tree, (params, i)) =>
              if(params.head.isImplicit) {
                tree
              }else{
                Apply(
                  tree,
                  params.zipWithIndex.map{ case (a, j) =>
                    if((i, j) == index){
                      q"self"
                    }else{
                      q"$a"
                    }
                  }
                )
              }
          }

          DefDef(
            NoMods,
            method.name,
            method.typeParams.map(t => symbol2typeDef(t)).filter(_.name.toString != "A"),
            newParams.map(_.map(s => symbol2valdef(s))),
            TypeTree(method.returnType),
            body
          )
        }

        val newClassDef = ClassDef(
          classDef.mods,
          classDef.name,
          classDef.tparams,
          Template(
            classDef.impl.parents,
            classDef.impl.self,
            classDef.impl.body ::: newMethods.toList
          )
        )

        newClassDef :: tail
      case other =>
        c.abort(c.enclosingPosition, "Must annotate a class or trait")
    }

    val expr = c.Expr[Any](Block(outputs, Literal(Constant(()))))
    println(expr.tree)
    expr
  }
}

