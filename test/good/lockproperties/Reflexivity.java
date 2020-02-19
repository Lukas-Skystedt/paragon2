public class Reflexivity
{
  static final se.chalmers.paragon.Lock A = se.chalmers.paragon.Lock.newLock("A", 2);
  static {
           A.addClause(se.chalmers.paragon.ActorList.newActorList(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Actor.newActorVariable(0)));
         }
  public static final java.lang.Object alice = new java.lang.Object();
  void foo()
  {
    int a;
    int b;
    a = b;
  }
}