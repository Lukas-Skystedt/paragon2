public class Guess
{
  public final java.lang.Object act;
  static final private se.chalmers.paragon.Lock Givehint = se.chalmers.paragon.Lock.newLock("Givehint", 0);
  public static final se.chalmers.paragon.Policy low = se.chalmers.paragon.Policy.newPolicy("low", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0)));
  public static final se.chalmers.paragon.Policy owner = se.chalmers.paragon.Policy.newPolicy("owner", se.chalmers.paragon.Policy.newPClause(se.chalmers.paragon.Actor.newActorVariable(0), se.chalmers.paragon.Atom.newAtom(Givehint)), se.chalmers.paragon.Policy.newPClause(act));
  public static final int FAILED = -2;
  public static final int LOWER = -1;
  public static final int CORRECT = 0;
  public static final int HIGHER = 1;
  private int number;
  private int tries;
  public Guess (final java.lang.Object act, int n)
  {
    this.act = act;
    this.number = n;
    this.tries = 0;
  }
  public int getNumber()
  {
    return number;
  }
  public int guess(int i)
  {
    this.tries += 1;
    if (this.tries > 10)
    return FAILED;
    int diff;
    {
      diff = this.number - i;
    }
    if (diff > 0)
    return HIGHER;
    if (diff < 0)
    return LOWER;
    return CORRECT;
  }
}