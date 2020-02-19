public class UnaryActor
{
  public static final java.lang.Object alice = new java.lang.Object();
  static final public se.chalmers.paragon.Lock myLock = se.chalmers.paragon.Lock.newLock("myLock", 1);
  public static final se.chalmers.paragon.Policy p = se.chalmers.paragon.Policy.newPolicy("p", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Atom.newAtom(myLock, se.chalmers.paragon.Actor.newActorVariable(0))));
  public static final se.chalmers.paragon.Policy apol = se.chalmers.paragon.Policy.newPolicy("apol", se.chalmers.paragon.Policy.newPClause(alice));
  public static void foo()
  {
    int pData = 42;
    int aliceData = 0;
    if (myLock.isOpen(alice))
    {
      aliceData = pData;
    }
  }
}