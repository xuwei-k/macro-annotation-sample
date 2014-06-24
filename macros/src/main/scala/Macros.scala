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
      case (cd @ ClassDef(_, cName, types, templates)) :: tail =>
        val createMethod =
          templates.children.collectFirst {
            case DefDef(_, _, _, vals :: _, _, _) =>
              q"""
                def create(..${vals})(implicit system: akka.actor.ActorSystem): akka.actor.ActorRef = {
                  system.actorOf(akka.actor.Props(classOf[${cName.toTypeName}], ..${vals.map(_.name)}))
                }"""
          }.getOrElse{
            c.abort(c.enclosingPosition, s"$cName does not have a case class constructor?")
          }
        // println(showCode(createMethod))

        val mod0: ModuleDef = tail match {
          case (md @ ModuleDef(_, mName, mTemp)) :: Nil 
            if cName.decodedName.toString == mName.decodedName.toString => md

          case Nil =>
            val cMod  = cd.mods
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

        cd :: mod1 :: Nil

      case other =>
        c.abort(c.enclosingPosition, "Must annotate a class or trait")
    }

    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }
}

