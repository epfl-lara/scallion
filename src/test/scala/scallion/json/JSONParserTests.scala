
package scallion.json

import org.scalatest._

class JSONParserTests extends FlatSpec with Inside {

  def parse(text: String): Option[Value] =
    JSONParser(JSONLexer(text.toIterator)).getValue

  def rest(text: String): JSONParser.Syntax[Value] =
    JSONParser(JSONLexer(text.toIterator)).syntax

  "JSON Parser" should "parse some basic examples" in {

    inside(parse("1.0")) {
      case Some(NumberValue(value, position)) => {
        assert(value == 1.0)
        assert(position == (0, 3))
      }
    }

    inside(parse("true")) {
      case Some(BooleanValue(value, position)) => {
        assert(value == true)
        assert(position == (0, 4))
      }
    }

    inside(parse("null")) {
      case Some(NullValue(position)) => {
        assert(position == (0, 4))
      }
    }

    inside(parse("[]")) {
      case Some(ArrayValue(values, position)) => {
        assert(values.size == 0)
        assert(position == (0, 2))
      }
    }

    inside(parse("{}")) {
      case Some(ObjectValue(values, position)) => {
        assert(values.size == 0)
        assert(position == (0, 2))
      }
    }

    inside(parse("""  "Hello World!" """)) {
      case Some(StringValue(value, position)) => {
        assert(value == "Hello World!")
        assert(position == (2, 16))
      }
    }

    inside(parse("""{"hello": null, "hi": "there"}""")) {
      case Some(ObjectValue(values, position)) => {
        assert(values.size == 2)

        inside(values(0)) {
          case (StringValue(key, keyPosition), NullValue(position)) => {
            assert(key == "hello")
          }
        }

        inside(values(1)) {
          case (StringValue(key, keyPosition), StringValue(value, position)) => {
            assert(key == "hi")
            assert(value == "there")
          }
        }
      }
    }

    inside(parse("""["Hello", null, [42, {}]]""")) {
      case Some(ArrayValue(values, position)) => {
        assert(values.size == 3)

        inside(values(0)) {
          case StringValue(value, position) => {
            assert(value == "Hello")
          }
        }

        inside(values(1)) {
          case NullValue(position) => ()
        }

        inside(values(2)) {
          case ArrayValue(values, position) => {
            assert(values.size == 2)

            inside(values(0)) {
              case NumberValue(value, position) => {
                assert(value == 42)
              }
            }

            inside(values(1)) {
              case ObjectValue(values, position) => {
                assert(values.isEmpty)
              }
            }
          }
        }
      }
    }
  }


  "Completions" should "work for arrays" in {

    val completed = JSONParser.arrayValue.complete {
      case SeparatorClass(sep) => SeparatorToken(sep, (0, 0))
    }

    assert(completed.nullable == Some(ArrayValue(Seq(), (0, 0))))

  }

  it should "work for objects" in {
    val completed = JSONParser.objectValue.complete {
      case SeparatorClass(sep) => SeparatorToken(sep, (0, 0))
    }

    assert(completed.nullable == Some(ObjectValue(Seq(), (0, 0))))
  }

  it should "work for general values" in {
    val completed = JSONParser.value.complete {
      case NullClass => NullToken((0, 0))
    }

    assert(completed.nullable == Some(NullValue((0, 0))))
  }

  it should "work even after errors" in {

    val erroneous = rest("[1,}")
    // Parser should fail:   ^ here

    assert(erroneous.nullable.isEmpty)

    val smallTrails = erroneous.trails.takeWhile(_.size <= 2).toList
    assert(smallTrails.contains(Seq(StringClass, SeparatorClass(']'))))
    assert(smallTrails.contains(Seq(NumberClass, SeparatorClass(']'))))
    assert(smallTrails.contains(Seq(NullClass, SeparatorClass(']'))))

    val completed = erroneous.complete {
      case StringClass => StringToken("XXX", (0, 0))
      case SeparatorClass(sep) => SeparatorToken(sep, (0, 0))
    }

    inside(completed.nullable) {
      case Some(ArrayValue(values, _)) => {
        assert(values.size == 2)
        assert(values(0) == NumberValue(1, (1, 2)))
        assert(values(1) == StringValue("XXX", (0, 0)))
      }
    }
  }
}