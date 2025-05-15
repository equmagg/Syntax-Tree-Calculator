using System;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Xml.Linq;
using static Calculator.SyntaxTree;
using static System.Net.Mime.MediaTypeNames;

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
                //str = "3 + ln 14 ^ 2 * (2 - 1) + integral("2*xdx", 1, 2)";
                try
                {
                    Console.WriteLine(Calculate(str, true));
                }
                catch (Exception ex) { Console.WriteLine(ex.Message); }
            }

        }
        static double Calculate(string str, bool pritSyntaxTree = false)
        {
            var lexer = new Lexer(str);
            var parser = new Parser(lexer);
            Node tree = parser.Expr();
            if(pritSyntaxTree) tree.Print();
            var evaluator = new Evaluator();
            double result = evaluator.Evaluate(tree);
            return Math.Round(result, 10);
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
            MathMod,
            Comma,
            Identifier,
            String,
            End,
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
            private readonly Dictionary<string, Func<List<Node>, double>> _functions;

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
                _functions = new Dictionary<string, Func<List<Node>, double>>(StringComparer.OrdinalIgnoreCase)
                {
                    ["max"] = args => Math.Max(Evaluate(args[0]), Evaluate(args[1])),
                    ["min"] = args => Math.Min(Evaluate(args[0]), Evaluate(args[1])),
                    ["abs"] = args => Math.Abs(Evaluate(args[0])),
                    ["clamp"] = args => Math.Clamp(Evaluate(args[0]), Evaluate(args[1]), Evaluate(args[2])),
                    ["integral"] = args =>
                    {
                        if (args[0] is not StringNode sn)
                            throw new Exception("First argument to integral must be a string");
                        var a = Evaluate(args[1]);
                        var b = Evaluate(args[2]);
                        return Integral(sn.Value, a, b);
                    }

                };
            }

            public double Evaluate(Node node, Dictionary<string, double>? variables = null)
            {
                switch (node)
                {
                    case UnaryOpNode un:
                        var operand = Evaluate(un.Operand, variables);
                        if (_unaryOps.TryGetValue(un.Op.Type, out var uop))
                            return uop(operand);
                        else
                            throw new Exception($"Unknown unary operator: {un.Op.Type}");
                    case VariableNode varNode:
                        if (variables is null) throw new Exception($"No variables initialized");
                        else if (variables.TryGetValue(varNode.Name, out var val))
                            return val;
                        throw new Exception($"Unknown variable: {varNode.Name}");

                    case StringNode sn:
                        throw new Exception("Cannot evaluate string as number");
                    case NumberNode num:
                        return double.Parse(num.Value);

                    case BinOpNode bin:
                        var left = Evaluate(bin.Left, variables);
                        var right = Evaluate(bin.Right, variables);

                        if (_binaryOps.TryGetValue(bin.Op.Type, out var op))
                            return op(left, right);
                        else
                            throw new Exception($"Unknown binary operator: {bin.Op.Type}");
                    case FunctionNode func:
                        if (_functions.TryGetValue(func.Name, out var fn))
                            return fn(func.Arguments);
                        else
                            throw new Exception($"Unknown function: {func.Name}");




                    default:
                        throw new Exception($"Unsupported node: {node.GetType().Name}");
                }
            }
            private double Integral(string function, double a, double b, uint steps = 10000)
            {
                string varName = "x";
                if (function.Contains("d"))
                {
                    var index = function.IndexOf("d");
                    if (function.Length > index + 1)
                    {
                        varName = (function[index + 1]).ToString();
                        function = function.Replace("d" + varName, "");
                    }
                }
                var lexer = new Lexer(function);
                var parser = new Parser(lexer);
                Node tree = parser.Expr();
                
                double stepSize = (b - a) / steps;
                double sum = 0;
                
                for (int i = 0; i < steps; i++)
                {
                    double x = a + i * stepSize;

                    var variables = new Dictionary<string, double> { [varName] = x };
                    double fx = Evaluate(tree, variables);
                    sum += fx * stepSize;
                }
                return Math.Round(sum, 3);

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
                bool hasDot = false;

                while (char.IsDigit(_currentChar) || _currentChar == '.')
                {
                    if (_currentChar == '.')
                    {
                        if (hasDot) break;
                        hasDot = true;
                    }
                    result += _currentChar;
                    Advance();
                }

                if (result == "." || result == "") throw new Exception("Invalid number format");

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
                        case ',': Advance(); return new Token(TokenType.Comma);
                    }
                    if (MatchAhead("=="))
                    {
                        for (int i = 0; i < 2; i++) Advance();
                        return new Token(TokenType.Equal);
                    }
                    if (MatchAhead("!="))
                    {
                        for (int i = 0; i < 2; i++) Advance();
                        return new Token(TokenType.NotEqual);
                    }
                    if (MatchAhead("++"))
                    {
                        for (int i = 0; i < 2; i++) Advance();
                        return new Token(TokenType.Increment);
                    }
                    if (MatchAhead("--"))
                    {
                        for (int i = 0; i < 2; i++) Advance();
                        return new Token(TokenType.Decrement);
                    }
                    if (_currentChar == '"')
                    {
                        Advance();
                        var sb = new StringBuilder();
                        while (_currentChar != '\0' && _currentChar != '"')
                        {
                            sb.Append(_currentChar);
                            Advance();
                        }

                        if (_currentChar != '"')
                            throw new Exception("Unterminated string literal");

                        Advance();
                        return new Token(TokenType.String, sb.ToString());
                    }

                    if (char.IsLetter(_currentChar))
                    {
                        
                        string ident = "";
                        while (char.IsLetter(_currentChar))
                        {
                            ident += _currentChar;
                            Advance();
                        }

                        return ident switch
                        {
                            "sqrt" => new Token(TokenType.Sqrt),
                            "log" => new Token(TokenType.Log),
                            "ln" => new Token(TokenType.NaturalLog),
                            "sin" => new Token(TokenType.Sin),
                            "cos" => new Token(TokenType.Cos),
                            "tg" => new Token(TokenType.Tan),
                            "ctg" => new Token(TokenType.Ctan),
                            "mod" => new Token(TokenType.MathMod),
                            _ => new Token(TokenType.Identifier, ident)
                        };

                    }

                    

                    throw new Exception($"Unknown character: {_currentChar}");
                }

                return new Token(TokenType.EOF);
            }
        }
        public abstract class Node
        {
            public abstract void Print(string indent = "", bool isLast = true);
            public abstract string ToExpressionString();

        }

        public class NumberNode : Node
        {
            public string Value { get; }

            public NumberNode(string value) => Value = value;

            public override void Print(string indent = "", bool isLast = true)
            {
                Console.WriteLine($"{indent}└── {Value}");
            }
            public override string ToExpressionString() => Value;

        }
        public class StringNode : Node
        {
            public string Value { get; }

            public StringNode(string value)
            {
                Value = value;
            }
            public override void Print(string indent = "", bool isLast = true)
            {
                Console.WriteLine($"{indent}└── {Value}");
            }
            public override string ToExpressionString() => Value;
        }
        public class VariableNode : Node
        {
            public string Name { get; }
            public Node? Value { get; }
            public VariableNode(string name)
            {
                Name = name;
            }
            public override void Print(string indent = "", bool isLast = true)
            {
                Console.WriteLine($"{indent}└── var {Name}");
            }
            public override string ToExpressionString() => Name;
        }

        public class FunctionNode : Node
        {
            public string Name { get; }
            public List<Node> Arguments { get; }

            public FunctionNode(string name, List<Node> arguments)
            {
                Name = name;
                Arguments = arguments;
            }

            public override void Print(string indent = "", bool isLast = true)
            {
                Console.WriteLine($"{indent}└── Func: {Name}");
                string childIndent = indent + (isLast ? "    " : "│   ");
                for (int i = 0; i < Arguments.Count; i++)
                    Arguments[i].Print(childIndent, i == Arguments.Count - 1);
            }
            public override string ToExpressionString()
            {
                var argsStr = string.Join(", ", Arguments.Select(a => a.ToExpressionString()));
                return $"{Name}({argsStr})";
            }

        }
        public class UnaryOpNode : Node
        {
            public Token Op { get; }
            public Node Operand { get; }

            public UnaryOpNode(Token op, Node operand)
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
            public override string ToExpressionString()
            {
                return Op.Type switch
                {
                    TokenType.MinusUnary or TokenType.Minus => "-" + Operand.ToExpressionString(),
                    TokenType.Plus => "+" + Operand.ToExpressionString(),
                    _ => $"{Op.Type}({Operand.ToExpressionString()})"
                };
            }

        }
        public class BinOpNode : Node
        {
            public Node Left { get; }
            public Token Op { get; }
            public Node Right { get; }

            public BinOpNode(Node left, Token op, Node right)
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
            public override string ToExpressionString()
            {
                return $"({Left.ToExpressionString()} {(Op.Type)} {Right.ToExpressionString()})";
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
            private Node ParseBinary(int minPrecedence, Func<Node> parseOperand)
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
            private Node Factor()
            {
                if (_currentToken.Type == TokenType.Identifier)
                {
                    var funcName = _currentToken.Value;
                    Eat(TokenType.Identifier);

                    if (_currentToken.Type == TokenType.LParen)
                    {
                        Eat(TokenType.LParen);
                        var args = new List<Node>();

                        if (_currentToken.Type != TokenType.RParen)
                        {
                            args.Add(Expr());
                            while (_currentToken.Type == TokenType.Comma)
                            {
                                Eat(TokenType.Comma);
                                args.Add(Expr());
                            }
                        }

                        Eat(TokenType.RParen);
                        return new FunctionNode(funcName, args);
                    }
                    return new VariableNode(funcName);
                    //throw new Exception($"Unexpected token after identifier '{funcName}' — expected '('");
                }

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
                if (_currentToken.Type == TokenType.String)
                {
                    var token = _currentToken;
                    Eat(TokenType.String);
                    return new StringNode(token.Value);
                }
                throw new Exception($"Unexpected token: {_currentToken.Type}");
            }

            public Node Expr()
            {
                return ParseBinary(0, Factor);
            }


        }

    }
}
