# Tokenizer module

The Tokenizer module is responsible for taking the source code represented as a string of characters and turning that into an array of tokens.

## Public API

```pseudocode
import Token from common

function tokenize(input: string) -> Token[]
```

## Implementation

### Cursor class

The private `Cursor` class manages position tracking and character navigation through the input string.

```pseudocode
class Cursor(input: string):
  function peek() -> char
  function advance() -> void
  function advanceN(n: int) -> void
  /* Return the current line and column (1-indexed) */
  function getPosition() -> Position
  function isAtEnd() -> boolean
  /* IMPORTANT: This method should ONLY check if the string matches at the current position.
     It should NOT advance the cursor. Advancing must be done separately with advance() or advanceN(). */
  function match(s: string) -> boolean
```

### TokenBuilder class

The private `TokenBuilder` class holds the mutable state for the current token and attribute. All properties should be private to the class and the only way to access or mutate the state is via the methods.

```pseudocode
class TokenBuilder():
  tokenValue: string = ""
  tokenType: TokenType = TokenType.Text
  tokenStart: Position = Position(1, 1)
  tokenAttributes: Attribute[] = []
  attributeName: string = ""
  attributeValue: string = ""
  attributeStart: Position = Position(1, 1)
  tokens: Token[] = []
  
  function pushCurrentToken(cursor: Cursor):
    this.tokens.push(Token(
      this.tokenType,
      this.tokenValue,
      this.tokenAttributes,
      Range(this.tokenStart, cursor.getPosition())
    ))
    this.tokenType = TokenType.Text
    this.tokenValue = ""
    this.tokenAttributes = []
    this.tokenStart = cursor.getPosition()

  function pushCurrentAttribute(cursor: Cursor):
    this.tokenAttributes.push(Attribute(
      this.attributeName,
      this.attributeValue,
      Range(this.attributeStart, cursor.getPosition())
    ))
    this.attributeName = ""
    this.attributeValue = ""
    this.attributeStart = cursor.getPosition()
  
  function appendToCurrentTokenValue(s: string):
    this.tokenValue += s
    
  function appendToCurrentAttributeName(s: string):
    this.attributeName += s
    
  function appendToCurrentAttributeValue(s: string):
    this.attributeValue += s

  function setCurrentAttributeStart(pos: Position):
    this.attributeStart = pos

  function getCurrentTokenValue():
    return this.tokenValue

  function setCurrentTokenType(t: TokenType):
    this.tokenType = t

  function pushErrorToken(message: string, cursor: Cursor):
    this.tokenType = TokenType.Error
    this.tokenValue = message
    this.pushCurrentToken(cursor)

  function getTokens():
    return this.tokens
```

### The tokenizer should be implemented as a state machine

There should be an enum called `TokenizerState` that represents the possible states that the tokenizer can be in. For a complete list of these states, see `02-tokenizer-state-machine.dot`. This enum should not be exported from the Tokenizer module.

```pseudocode
enum TokenizerState(
  TEXT,
  TAG_OPEN,
  ...
)
```

The tokenize function should contain a single switch statement that models the state machine.

```pseudocode
function tokenize(input: string) -> Token[]:
  let cursor = new Cursor(input)
  let builder = new TokenBuilder()
  let state = TokenizerState.TEXT
  let doctypeNameBuffer = ""
  let storedTagName = ""
  while !this.cursor.isAtEnd():
    let char = this.cursor.peek()
    switch this.state:
      case TokenizerState.TEXT:
        // ...
      case TokenizerState.TAG_OPEN:
        // ...
        
  return builder.getTokens()
```

There should be a one-to-one mapping between the switch cases and the different values of the `TokenizerState` enum.

### HTML entities

The tokenizer should not need to treat HTML character entities (e.g., `&amp;`, `&lt;`, `&#39;`, `&#x27;`) in any special way and should preserve them as-is in token values.

### Token positions

The token start position is inclusive while the token end position is exclusive.

For example, a token such as `<div>` is expected to have a start position equal to the position of the `<` and an end position one past the `>`. This means that the tokenizer needs to advance before pushing the current token when seeing a `>` while parsing the tag name.

### Error recovery

The tokenizer must perform error recovery when there is no valid transition.

Error recovery works by emitting an error token and then returning to the initial state (i.e. the state TEXT).

The tokenizer should always advance before pushing an error token to avoid
infinite loops.

### RAWTEXT_DATA state implementation

The RAWTEXT_DATA state requires special handling for end tag creation. When matching the closing tag pattern `</{storedTagName}>`:

1. **Check the pattern** using `cursor.match()` WITHOUT advancing the cursor
2. If matched, push the current text token (if not empty)
3. **Initialize a new token** to set the correct start position
4. **Set token type** to EndTag and **append the stored tag name**
5. **Advance the cursor** explicitly by the length of the matched end tag
6. **Push the end tag token**

**Critical**: The cursor must not be advanced during the pattern check, only after the token is properly initialized. This ensures correct position tracking for the end tag token.

### Example

The document

```hop
<h1></h1>
<h2></h2>
```

should yield the following token types and positions:

```
StartTag 1:1-1:5
EndTag 1:5-1:10
Text 1:10-2:1
StartTag 2:1-2:5
EndTag 2:5-2:10
Text 2:10-3:1
```
