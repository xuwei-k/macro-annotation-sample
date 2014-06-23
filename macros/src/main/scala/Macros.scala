import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox._
import language.experimental.macros

trait Foo[A]

class mkCompanion extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro mkCompanionMacro.impl
}
object mkCompanionMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val inputs : List[Tree] = annottees.map(_.tree)(collection.breakOut)
    val outputs: List[Tree] = inputs match {
      case (cd @ ClassDef(_, cName, _, _)) :: tail =>
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

        val cTpe        = Ident(cd.name.decodedName)
        val fooDef      = q"implicit def hasFoo: Foo[$cTpe] = new Foo[$cTpe]{}"
        val mTempBody1  = fooDef :: mTempBody0
        val mTemp1      = Template(mTempParents, mTempSelf, mTempBody1)
        val mod1        = ModuleDef(mod0.mods, mod0.name, mTemp1)

        cd :: mod1 :: Nil

      case _ => c.abort(c.enclosingPosition, "Must annotate a class or trait")
    }

    c.Expr[Any](Block(outputs, Literal(Constant(()))))
  }
}

