public class PossibilityInferred
{
  private static final java.lang.Object Alice = new java.lang.Object();
  public static final se.chalmers.paragon.Policy alice = se.chalmers.paragon.Policy.newPolicy("alice", se.chalmers.paragon.Policy.newPClause(Alice));
  public static final se.chalmers.paragon.Policy bot = se.chalmers.paragon.Policy.newPolicy("bot", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0)));
  private int aa;
  private int bb;
  public void method0()
  {
    int a = 5;
    int b = 4;
    a = b + 2;
    aa = bb + 2;
  }
}