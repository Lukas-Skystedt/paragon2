public class Good1
{
  public static final java.lang.Object Alice = new java.lang.Object();
  static final public se.chalmers.paragon.Lock Declassified = se.chalmers.paragon.Lock.newLock("Declassified", 0);
  public static final se.chalmers.paragon.Policy alice = se.chalmers.paragon.Policy.newPolicy("alice", se.chalmers.paragon.Policy.newPClause(Alice));
  public static final se.chalmers.paragon.Policy pub = se.chalmers.paragon.Policy.newPolicy("pub", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0)));
  public static final int pubData;
  public static final int aliceData;
  public static void foo()
  {
    aliceData = pubData;
  }
}