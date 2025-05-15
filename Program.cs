using static Calculator.SyntaxTree;

namespace Calculator
{
    internal class Program
    {
        static void Main(string[] args)
        {
            for (; ; )
            {
                var str = Console.ReadLine();
                str ??= "0";
                if (!CheckBrackets(str)) { Console.WriteLine("Closing bracket expected"); continue; }
                if (str == "exit" || str == "quit" || str == "q") break;
                //str = "3 + sqrt 14 ^ 2 * (2 - 1)";
                try
                {
                    Console.WriteLine(Calculate(str));
                }
                catch (Exception ex) { Console.WriteLine(ex.Message); }
            }

        }
        static double Calculate(string str, bool pritSyntaxTree = false)
        {
            var lexer = new Lexer(str);
            var parser = new Parser(lexer);
            AstNode tree = parser.Expr();
            if(pritSyntaxTree) tree.Print();
            var evaluator = new Evaluator();
            double result = evaluator.Evaluate(tree);
            return result;
        }
        public static bool CheckBrackets(string input)
        {
            var stack = new Stack<char>();
            bool IsMatchingPair(char open, char close)
            {
                return (open == '(' && close == ')')
                    || (open == '[' && close == ']')
                    || (open == '{' && close == '}');
            }
            foreach (var ch in input)
            {
                if (ch == '(' || ch == '[' || ch == '{')
                {
                    stack.Push(ch);
                }
                else if (ch == ')' || ch == ']' || ch == '}')
                {
                    if (stack.Count == 0)
                        return false;

                    char open = stack.Pop();

                    if (!IsMatchingPair(open, ch))
                        return false;
                }
            }

            return stack.Count == 0;
        }

    }

    public class SyntaxTree
    {
        public enum TokenType
        {
            Number,
            Plus,
            Minus,
            Mul,
            Div,
            LParen,
            RParen,
            EOF,
            Pow,
            Equal, 
            NotEqual,
            Sqrt,
            Increment,
            Decrement,
            MinusUnary,
            Log,
            NaturalLog,
            Sin,
            Cos,
            Tan,
            Ctan,
            DivisionMod,
            MathMod
        }
        public enum Associativity
        {
            Left,
            Right
        }
        public class OperatorInfo
        {
            public int Precedence { get; }
            public Associativity Associativity { get; }

            public OperatorInfo(int precedence, Associativity associativity)
            {
                Precedence = precedence;
                Associativity = associativity;
            }
        }
        public static class OperatorTable
        {
            public static readonly Dictionary<TokenType, OperatorInfo> BinaryOperators = new()
            {
                [TokenType.Plus] = new OperatorInfo(1, Associativity.Left),
                [TokenType.Minus] = new OperatorInfo(1, Associativity.Left),
                [TokenType.Mul] = new OperatorInfo(2, Associativity.Left),
                [TokenType.Div] = new OperatorInfo(2, Associativity.Left),
                [TokenType.DivisionMod] = new OperatorInfo(2, Associativity.Left),
                [TokenType.Pow] = new OperatorInfo(3, Associativity.Right),
                [TokenType.Equal] = new OperatorInfo(0, Associativity.Left),
                [TokenType.NotEqual] = new OperatorInfo(0, Associativity.Left),
                [TokenType.Log] = new OperatorInfo(3, Associativity.Right),
            };
            public static readonly HashSet<TokenType> UnaryOperators = new()
            {
                TokenType.Plus,
                TokenType.Minus,
                TokenType.Sqrt,
                TokenType.NaturalLog,
                TokenType.Sin,
                TokenType.Cos,
                TokenType.Tan,
                TokenType.Ctan,
                TokenType.MathMod,
            };

        }
        public class Token
        {
            public TokenType Type { get; }
            public string Value { get; }

            public Token(TokenType type, string value = null)
            {
                Type = type;
                Value = value;
            }

            public override string ToString() => $"{Type}: {Value}";
        }
        public class Evaluator
        {
            private readonly Dictionary<TokenType, Func<double, double, double, double>> _tetraryOps;
            private readonly Dictionary<TokenType, Func<double, double, double>> _binaryOps;
            private readonly Dictionary<TokenType, Func<double, double>> _unaryOps;
            public Evaluator()
            {
                _binaryOps = new Dictionary<TokenType, Func<double, double, double>>
                {
                    [TokenType.Plus] = (a, b) => a + b,
                    [TokenType.Minus] = (a, b) => a - b,
                    [TokenType.Mul] = (a, b) => a * b,
                    [TokenType.Div] = (a, b) => a / b,
                    [TokenType.Pow] = Math.Pow,
                    [TokenType.Log] = Math.Log,
                    [TokenType.DivisionMod] = (a, b) => a % b,
                    [TokenType.Equal] = (a, b) => (a == b ? 1 : 0),
                };
                _unaryOps = new Dictionary<TokenType, Func<double, double>>
                {
                    [TokenType.Increment] = x => +x,
                    [TokenType.Decrement] = x => -x,
                    [TokenType.Sqrt] = Math.Sqrt,
                    [TokenType.NaturalLog] = Math.Log,
                    [TokenType.Sin] = Math.Sin,
                    [TokenType.Cos] = Math.Cos,
                    [TokenType.Tan] = Math.Tan,
                    [TokenType.Ctan] = Math.Atan,
                    [TokenType.MathMod] = (a) => (a<0 ? 0-a : a)
                };

        }

        public double Evaluate(AstNode node)
            {
                switch (node)
                {
                    case UnaryOpNode un:
                        var operand = Evaluate(un.Operand);
                        if (_unaryOps.TryGetValue(un.Op.Type, out var uop))
                            return uop(operand);
                        else
                            throw new Exception($"Unknown unary operator: {un.Op.Type}");

                    case NumberNode num:
                        return double.Parse(num.Value);

                    case BinOpNode bin:
                        var left = Evaluate(bin.Left);
                        var right = Evaluate(bin.Right);

                        if (_binaryOps.TryGetValue(bin.Op.Type, out var op))
                            return op(left, right);
                        else
                            throw new Exception($"Unknown binary operator: {bin.Op.Type}");

                    default:
                        throw new Exception($"Unsupported node: {node.GetType().Name}");
                }
            }
        }

        public class Lexer
        {
            private readonly string _text;
            private int _pos;
            private char _currentChar;

            public Lexer(string text)
            {
                _text = text;
                _pos = 0;
                _currentChar = _text.Length > 0 ? _text[0] : '\0';
            }

            private void Advance()
            {
                _pos++;
                _currentChar = _pos < _text.Length ? _text[_pos] : '\0';
            }

            private void SkipWhitespace()
            {
                while (char.IsWhiteSpace(_currentChar)) Advance();
            }
            private char Peek()
            {
                return _pos + 1 < _text.Length ? _text[_pos + 1] : '\0';
            }
            private char Peek(int pos)
            {
                return _pos + pos < _text.Length ? _text[_pos + pos] : '\0';
            }
            private bool MatchAhead(string text)
            {
                for (int i = 0; i < text.Length; i++)
                {
                    if (Peek(i) != text[i])
                        return false;
                }
                return true;
            }

            private Token Number()
            {
                string result = "";
                while (char.IsDigit(_currentChar))
                {
                    result += _currentChar;
                    Advance();
                }
                return new Token(TokenType.Number, result);
            }

            public Token GetNextToken()
            {
                while (_currentChar != '\0')
                {
                    if (char.IsWhiteSpace(_currentChar))
                    {
                        SkipWhitespace();
                        continue;
                    }

                    if (char.IsDigit(_currentChar)) return Number();
                    if (_currentChar == '=' && Peek() == '=')
                    {
                        Advance(); Advance();
                        return new Token(TokenType.Equal);
                    }
                    if (_currentChar == '+' && Peek() == '+')
                    {
                        Advance(); Advance();
                        return new Token(TokenType.Increment);
                    }
                    if (_currentChar == '-' && Peek() == '-')
                    {
                        Advance(); Advance();
                        return new Token(TokenType.Decrement);
                    }
                    if (MatchAhead("sqrt"))
                    {
                        for (int i = 0; i < 4; i++) Advance();
                        return new Token(TokenType.Sqrt);
                    }
                    if (MatchAhead("log"))
                    {
                        for (int i = 0; i < 3; i++) Advance();
                        return new Token(TokenType.Log);
                    }
                    if (MatchAhead("ln"))
                    {
                        for (int i = 0; i < 2; i++) Advance();
                        return new Token(TokenType.NaturalLog);
                    }
                    if (MatchAhead("sin"))
                    {
                        for (int i = 0; i < 3; i++) Advance();
                        return new Token(TokenType.Sin);
                    }
                    if (MatchAhead("cos"))
                    {
                        for (int i = 0; i < 3; i++) Advance();
                        return new Token(TokenType.Cos);
                    }
                    if (MatchAhead("tg"))
                    {
                        for (int i = 0; i < 2; i++) Advance();
                        return new Token(TokenType.Tan);
                    }
                    if (MatchAhead("ctg"))
                    {
                        for (int i = 0; i < 3; i++) Advance();
                        return new Token(TokenType.Ctan);
                    }
                    if (MatchAhead("mod"))
                    {
                        for (int i = 0; i < 3; i++) Advance();
                        return new Token(TokenType.MathMod);
                    }
                    switch (_currentChar)
                    {
                        case '+': Advance(); return new Token(TokenType.Plus);
                        case '-': Advance(); return new Token(TokenType.Minus);
                        case '*': Advance(); return new Token(TokenType.Mul);
                        case '/': Advance(); return new Token(TokenType.Div);
                        case '(': Advance(); return new Token(TokenType.LParen);
                        case ')': Advance(); return new Token(TokenType.RParen);
                        case '^': Advance(); return new Token(TokenType.Pow);
                        case '%': Advance(); return new Token(TokenType.DivisionMod);
                    }

                    throw new Exception($"Unknown character: {_currentChar}");
                }

                return new Token(TokenType.EOF);
            }
        }
        public abstract class AstNode
        {
            public abstract void Print(string indent = "", bool isLast = true);
        }

        public class NumberNode : AstNode
        {
            public string Value { get; }

            public NumberNode(string value) => Value = value;

            public override void Print(string indent = "", bool isLast = true)
            {
                Console.WriteLine($"{indent}└── {Value}");
            }
        }
        public class UnaryOpNode : AstNode
        {
            public Token Op { get; }
            public AstNode Operand { get; }

            public UnaryOpNode(Token op, AstNode operand)
            {
                Op = op;
                Operand = operand;
            }
            public override void Print(string indent = "", bool isLast = true)
            {
                Console.WriteLine($"{indent}└── {Op.Type}");
                string childIndent = indent + (isLast ? "    " : "│   ");
                Operand.Print(childIndent, true);
            }
        }
        public class BinOpNode : AstNode
        {
            public AstNode Left { get; }
            public Token Op { get; }
            public AstNode Right { get; }

            public BinOpNode(AstNode left, Token op, AstNode right)
            {
                Left = left;
                Op = op;
                Right = right;
            }

            public override void Print(string indent = "", bool isLast = true)
            {
                Console.WriteLine($"{indent}└── {Op.Type}");
                string childIndent = indent + (isLast ? "    " : "│   ");
                Left.Print(childIndent, false);
                Right.Print(childIndent, true);
            }
        }
        public class Parser
        {
            private readonly Lexer _lexer;
            private Token _currentToken;

            public Parser(Lexer lexer)
            {
                _lexer = lexer;
                _currentToken = _lexer.GetNextToken();
            }

            private void Eat(TokenType type)
            {
                if (_currentToken.Type == type)
                    _currentToken = _lexer.GetNextToken();
                else
                    throw new Exception($"Expected {type} got {_currentToken.Type}");
            }
            private AstNode ParseBinary(int minPrecedence, Func<AstNode> parseOperand)
            {
                var left = parseOperand();

                while (OperatorTable.BinaryOperators.TryGetValue(_currentToken.Type, out var opInfo))
                {
                    int precedence = opInfo.Precedence;
                    Associativity assoc = opInfo.Associativity;

                    bool shouldParse = assoc == Associativity.Left
                        ? precedence >= minPrecedence
                        : precedence > minPrecedence;

                    if (!shouldParse) break;

                    var opToken = _currentToken;
                    Eat(_currentToken.Type);
                    var right = ParseBinary(precedence + (assoc == Associativity.Left ? 1 : 0), parseOperand);
                    left = new BinOpNode(left, opToken, right);
                }

                return left;
            }
            private AstNode Factor()
            {
                if (OperatorTable.UnaryOperators.Contains(_currentToken.Type))
                {
                    var opToken = _currentToken;
                    Eat(opToken.Type);
                    var operand = Factor();
                    return new UnaryOpNode(opToken, operand);
                }

                if (_currentToken.Type == TokenType.Plus || _currentToken.Type == TokenType.Minus)
                {
                    var opToken = _currentToken;
                    Eat(opToken.Type);
                    var operand = Factor();
                    return new UnaryOpNode(opToken, operand);
                }

                if (_currentToken.Type == TokenType.Number)
                {
                    var token = _currentToken;
                    Eat(TokenType.Number);
                    return new NumberNode(token.Value);
                }

                if (_currentToken.Type == TokenType.LParen)
                {
                    Eat(TokenType.LParen);
                    var node = Expr();
                    Eat(TokenType.RParen);
                    return node;
                }

                throw new Exception($"Unexpected token: {_currentToken.Type}");
            }

            public AstNode Expr()
            {
                return ParseBinary(0, Factor);
            }


        }

    }
}
