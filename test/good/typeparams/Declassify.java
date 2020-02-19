class Declassify
{
  static final private se.chalmers.paragon.Lock Declassify = se.chalmers.paragon.Lock.newLock("Declassify", 0);
  private static final se.chalmers.paragon.Policy dec = se.chalmers.paragon.Policy.newPolicy("dec", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Atom.newAtom(Declassify)));
  public static <A> A declassify(final se.chalmers.paragon.Policy TO, A x)
  {
    {
      return x;
    }
  }
}