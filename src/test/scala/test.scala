import org.scalatest.FunSuite

import scala.collection.{mutable, immutable}

class SimpleCastTest extends FunSuite
{
    def lexicalCast[T]( a : String )( implicit conv : String => T ) = conv(a)
    
    implicit def stringToLong( s : String ) : Long = augmentString(s).toLong
    implicit def stringToDouble( s : String ) : Double = augmentString(s).toDouble
    implicit def stringToString( s : String ) : String = s
    
    test("Foo")
    {
        val a = "1"
        val b = "2.0"
        val c = "Foom"
        
        val p = lexicalCast[Long](a)
        val q = lexicalCast[Double](b)
        val v = lexicalCast[String](c)
        
        assert( p === 1 )
        assert( q === 2.0 )
        assert( v === "Foom" )
    }
}


trait OArchive
{
    def save[T]( el : Serializer[T] )
    
    def save( v : Int ) : Unit
    def save( v : Long ) : Unit
    def save( v : Double ) : Unit
}

trait IArchive
{
    def load[T] : T
}

trait Serializer[T]
{
    def save( oa : OArchive ) : Unit
    def load( ia : IArchive ) : T
}


class BinaryOArchive( val fileName : String ) extends OArchive
{
    def save[T]( el : Serializer[T] ) = el.save(this)
    
    def save( v : Int ) {}
    def save( v : Long ) {}
    def save( v : Double ) {}
}

class Simple1( val a : Int, val b : Long, val c : Double )

object StandardSerializers
{
    implicit def tuple2Serializer[A <% Serializer[A], B <% Serializer[B]]( t : (A, B) ) = new Serializer[(A, B)]
    {
        def save( oa : OArchive ) =
        {
            oa.save( t._1 )
            oa.save( t._2 )
        }
        def load( ia : IArchive ) = (ia.load[A], ia.load[B])
    }
    
    implicit def tuple3Serializer[A <% Serializer[A], B <% Serializer[B], C <% Serializer[C]]( t : (A, B, C) ) = new Serializer[(A, B, C)]
    {
        def save( oa : OArchive ) =
        {
            oa.save( t._1 )
            oa.save( t._2 )
            oa.save( t._3 )
        }
        def load( ia : IArchive ) = (ia.load[A], ia.load[B], ia.load[C])
    }
    
    implicit def listSerializer[A <% Serializer[A]]( l : List[A] ) = new Serializer[List[A]]
    {
        def save( oa : OArchive ) =
        {
            oa.save( l.size )
            l.foreach( el => oa.save(el) )
        }
        
        def load( ia : IArchive ) =
        {
            val lb = mutable.ListBuffer[A]()
            
            val l = ia.load[Int]
            
            (0 until l).map( i => ia.load[A] ).toList
        }
    }
}

object MySerializers
{
    implicit def simple1Serializer( s : Simple1 ) = new Serializer[Simple1]
    {
        def save( oa : OArchive ) =
        {
            oa.save(s.a)
            oa.save(s.b)
            oa.save(s.c)
        }
        
        def load( ia : IArchive ) = new Simple1( ia.load[Int], ia.load[Long], ia.load[Double] )
    }
}

class Test extends FunSuite
{
    test("Simple test")
    {
        import StandardSerializers._
        import MySerializers._
        
        val oa = new BinaryOArchive( "foo.bin" )
        
        val a = new Simple1( 4, 5L, 3.0 )
        
        // Here we should serialize the data in a
        oa.save(a)
        // Here we should serialize a reference to the previous a via some foo
        oa.save(a)
        
        val b = new Simple1( 5, 6L, 7.0 )
        val c = new Simple1( 8, 9L, 10.0 )
        val d = (b, c)
        
        oa.save( d )
        
        val e = (a, b, c)
        oa.save(e)
        
        val f = List(a, b, c)
        val g = Array(a, b, c)
        oa.save(f)
    }
}





        
