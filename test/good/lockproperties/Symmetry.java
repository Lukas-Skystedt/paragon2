public class Symmetry
{
  static final se.chalmers.paragon.Lock A = se.chalmers.paragon.Lock.newLock("A", 2);
  static {
           A.addClause(se.chalmers.paragon.ActorList.newActorList(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Actor.newActorVariable(1)), se.chalmers.paragon.Atom.newAtom(A, se.chalmers.paragon.Actor.newActorVariable(1), se.chalmers.paragon.Actor.newActorVariable(0)));
         }
  public static final java.lang.Object alice = new java.lang.Object();
  public static final java.lang.Object bob = new java.lang.Object();
  void foo()
  {
    int a;
    int b;
    A.open(alice, bob);
    a = b;
  }
}