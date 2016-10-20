package xyz.hyperreal.mos6502


object Options
{
	def apply( args: Array[String] )( options: PartialFunction[List[String], List[String]] )
	{
		def nextOption( list: List[String] ): Unit =
			if (list != Nil)
				nextOption( options(list) )

		nextOption( args.toList )
	}
}