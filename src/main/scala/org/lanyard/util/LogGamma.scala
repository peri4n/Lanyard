package org.lanyard.util

object LogGamma extends ( Double => Double ) {

  private val coeficients = Array(
    57.1562356658629235, -59.5979603554754912, 14.1360979747417471,
    -0.491913816097620199, .339946499848118887e-4, .465236289270485756e-4,
    -.983744753048795646e-4, .158088703224912494e-3, -.210264441724104883e-3,
    .217439618115212643e-3, -.164318106536763890e-3, .844182239838527433e-4,
    -.261908384015814087e-4, .368991826595316234e-5 )

  def apply( value: Double ): Double = {

    require( value > 0, "Argument of LogGamma needs to be strictly positive. Found value: " + value )

    var y = value
    var tmp = value + 5.24218750000000000
    tmp = ( value + 0.5 ) * math.log( tmp ) - tmp
    var ser = 0.999999999999997092
    var j = 0
    while ( j < 14 ) {
      y += 1
      ser += coeficients( j ) / y
      j += 1
    }

    tmp + math.log( 2.5066282746310005 * ser / value )
  }

}
