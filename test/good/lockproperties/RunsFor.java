public class RunsFor
{
  static final se.chalmers.paragon.Lock ActsFor = se.chalmers.paragon.Lock.newLock("ActsFor", 2);
  static {
           ActsFor.addClause(se.chalmers.paragon.ActorList.newActorList(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Actor.newActorVariable(1)), se.chalmers.paragon.Atom.newAtom(ActsFor, se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Actor.newActorVariable(2)), se.chalmers.paragon.Atom.newAtom(ActsFor, se.chalmers.paragon.Actor.newActorVariable(2), se.chalmers.paragon.Actor.newActorVariable(1)));
         }
  static final se.chalmers.paragon.Lock RunsFor = se.chalmers.paragon.Lock.newLock("RunsFor", 1);
  static {
           RunsFor.addClause(se.chalmers.paragon.ActorList.newActorList(se.chalmers.paragon.Actor.newActorVariable(0)), se.chalmers.paragon.Atom.newAtom(RunsFor, se.chalmers.paragon.Actor.newActorVariable(1)), se.chalmers.paragon.Atom.newAtom(ActsFor, se.chalmers.paragon.Actor.newActorVariable(1), se.chalmers.paragon.Actor.newActorVariable(0)));
         }
  public static final java.lang.Object alice = new java.lang.Object();
  public static final java.lang.Object bob = new java.lang.Object();
  public static final java.lang.Object charlie = new java.lang.Object();
  void foo()
  {
    int a;
    int b;
    RunsFor.open(charlie);
    ActsFor.open(charlie, bob);
    ActsFor.open(bob, alice);
    a = b;
  }
}