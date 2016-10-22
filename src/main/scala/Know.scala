package xyz.hyperreal.mos6502


abstract class Know[+A] {
	def isEmpty: Boolean
	def get: A
	
	def isDefined = !isEmpty
	def getOrElse[B >: A]( default: => B ) = if (isEmpty) default else get
	def map[B]( f: A => B ) = if (isEmpty) this.asInstanceOf[Know[Nothing]] else Known( f(get) )
}

case class Known[+A]( x: A ) extends Know[A] {
	def isEmpty = false
	def get = x
}

case object Unknown extends Know[Nothing] {
	def isEmpty = true
	def get = throw new NoSuchElementException( "Unknown.get" )
}

case object Knowable extends Know[Nothing] {
	def isEmpty = true
	def get = throw new NoSuchElementException( "Knowable.get" )
}