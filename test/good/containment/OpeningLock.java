public class OpeningLock
{
  static final public se.chalmers.paragon.Lock A = se.chalmers.paragon.Lock.newLock("A", 0);
  static final public se.chalmers.paragon.Lock B = se.chalmers.paragon.Lock.newLock("B", 1);
  public static final se.chalmers.paragon.Policy p = se.chalmers.paragon.Policy.newPolicy("p", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Atom.newAtom(A)));
  public static final int foo()
  {
    int x = 4;
    int y = 5;
    A.open();
    x = y;
    return x;
  }
}