public class Basic
{
  static final public se.chalmers.paragon.Lock myLock = se.chalmers.paragon.Lock.newLock("myLock", 0);
  public static final se.chalmers.paragon.Policy p = se.chalmers.paragon.Policy.newPolicy("p", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Atom.newAtom(myLock)));
  public static final se.chalmers.paragon.Policy bot = se.chalmers.paragon.Policy.newPolicy("bot", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0)));
  public static void foo()
  {
    int pData = 42;
    int botData = 0;
    if (myLock)
    {
      botData = pData;
    }
  }
}