public class Good7
{
  public static final java.lang.Object Alice = new java.lang.Object();
  static final public se.chalmers.paragon.Lock Declassified = se.chalmers.paragon.Lock.newLock("Declassified", 1);
  public static final se.chalmers.paragon.Policy secret = se.chalmers.paragon.Policy.newPolicy("secret", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Atom.newAtom(Declassified, se.chalmers.paragon.Actor.newActorVariable(0))));
  public static final se.chalmers.paragon.Policy alice = se.chalmers.paragon.Policy.newPolicy("alice", se.chalmers.paragon.Policy.newPClause(Alice));
  public static final int aliceData;
  public static final int secretData;
  public static void bar()
  {
    Declassified.open(Alice);
    aliceData = secretData;
    Declassified.close(Alice);
  }
}