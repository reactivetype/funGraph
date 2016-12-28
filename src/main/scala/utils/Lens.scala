package utils

/**
  * Unidirectional lens for focusing on things
  * Created by Rindra on 9/6/2016.
  */

case class Lens[Big, Small](get: Big => Small, update : (Small => Small) => Big => Big) {

}

object Lens {
  /*
   * Create a lens that allows you to focus on a small thing of a bigger one
   */
  def focus[Big, Small](get: Big => Small, update: (Small => Small) => Big => Big) =
    Lens(get, update)

  /*
   * Get a small part of a bigger thing
   */
  def get[Big, Small](lens: Lens[Big, Small])(big: Big) = lens.get(big)

  /*
   * Set a small part of a bigger thing
   */
  def set[Big, Small](lens: Lens[Big, Small])(small: Small)(big: Big) = lens.update(const(small))(big)

  /*
   * Higher-order helpers
   */
  // Implicit for creating an operator (~>) used for composing two lenses
  implicit class LensComposeOp[Big, Small](largerFocus: Lens[Big, Small]) {
    def ~>[Smaller](smallerFocus: Lens[Small, Smaller]): Lens[Big, Smaller] = {
      def getter(b: Big): Smaller = smallerFocus.get(largerFocus.get(b))
      def updater(fn: Smaller => Smaller)(b: Big): Big = largerFocus.update(smallerFocus.update(fn))(b)
      Lens(getter,updater)
    }
  }

  // Creates a constant function that always returns the first argument
  // no matter what the value of its second argument is
  private def const[A,B](a: A) = (b: B) => a
}
