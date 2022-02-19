namespace Boogie.Lang.Core;

public enum BinaryOperatorCharacter
{

}

public enum UnaryOperatorType
{
    Not,
    Neg,
}
public enum BinaryOperatorType
{
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    IDiv,
    Pow,

    //DotProd,
    //CrossProd,

    DotIndex,
    Index,

    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,

    In,

    //PipeForward,

    And,
    Or,
}