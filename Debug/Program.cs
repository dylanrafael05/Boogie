using Boogie.Lang;
using Boogie.Lang.Core;

//////////////////////////////////
/// CONSTANTS
//////////////////////////////////
const string ExamplesPath = @"..\..\..\..\Examples";

//////////////////////////////////
/// LOCAL FUNCTIONS
//////////////////////////////////
void Print(string text, string end = "\n")
{
    Console.Write(text + end);
}

void PrintNewLine(int count = 1)
{
    for (int i = 0; i < count; i++)
    {
        Console.WriteLine("");
    }
}

string Input(string text)
{
    Console.WriteLine(text);
    Console.Write(":: ");
    return Console.ReadLine()!;
}

void Clear()
{
    Console.Clear();
}

void PrintExamplesList()
{
    Print("Available examples:");
    foreach (var f in Directory.EnumerateFiles(ExamplesPath))
    {
        var fInfo = new FileInfo(f);

        if (fInfo.Extension is ".bge" or ".boogie")
        {
            Print("  - " + fInfo.Name);
        }
    }
}

string ParseDebug(string sourcePath, string filename)
{
    try
    {
        var cons = new Parser(new Lexer(File.ReadAllText(sourcePath), filename)).Parse();
        return cons.GetDescription();
    }
    catch(SyntaxException syntaxException)
    {
        return "SyntaxException:\n" + syntaxException.Message;
    }
}

string LowerDebug(string sourcePath, string filename)
{
    try
    {
        var cons = new Parser(new Lexer(File.ReadAllText(sourcePath), filename)).Parse();
        var prog = ProgramBuilder.New().AddCons(cons).Build();
        return prog.GetDescription();
    }
    catch (SyntaxException syntaxException)
    {
        return "SyntaxException:\n" + syntaxException.Message;
    }
}

//////////////////////////////////
/// CONSTANTS
//////////////////////////////////
while (true)
{
    Print(@"
Welcome to the Boogie Debugger Tool!
This tool can help you develop or work with Boogie's backend with multiple commands:
 - test-parser [0]
 - update-test-data [1]
 - test-lowerer [2]
".Trim());
    Print("");

    var command = Input("Enter command:");
    Clear();

    switch (command)
    {
        case "test-parser" or "0":
        {
            while (true)
            {
                PrintExamplesList();
                PrintNewLine(2);

                var file = Input("Enter the file to update:");
                PrintNewLine(2);

                var path = Path.GetFullPath(Path.Combine(ExamplesPath, file));
                Print("The full path of this file is: " + path);

                if (!File.Exists(path))
                {
                    Print("Invalid example!");

                    continue;
                }
                var consDesc = ParseDebug(path, file);

                Console.Clear();
                Print("This file was parsed as such: \n");
                Print(consDesc);

                break;
            }

            break;
        }

        case "test-lowerer" or "2":
        {
            while (true)
            {
                PrintExamplesList();
                PrintNewLine(2);

                var file = Input("Enter the file to update:");
                PrintNewLine(2);

                var path = Path.GetFullPath(Path.Combine(ExamplesPath, file));
                Print("The full path of this file is: " + path);

                if (!File.Exists(path))
                {
                    Print("Invalid example!");

                    continue;
                }
                var consDesc = LowerDebug(path, file);

                Console.Clear();
                Print("This file was parsed as such: \n");
                Print(consDesc);

                break;
            }

            break;
        }

        case "update-test-data" or "1":
        {
            while (true)
            {
                PrintExamplesList();
                PrintNewLine(2);

                var file = Input("Enter the file to update:");
                PrintNewLine(2);

                var path = Path.GetFullPath(Path.Combine(ExamplesPath, file));
                Print("The full path of this file is: " + path);

                if (!File.Exists(path))
                {
                    Print("Invalid example!");

                    continue;
                }
                var fileInfo = new FileInfo(path);
                var consDesc = ParseDebug(path, file);

                Console.Clear();
                Print("This file was parsed as such: \n");
                Print(consDesc);
                Print("\nIs this acceptable to make the new correct parsing value? Enter 'y' if so.");

                var yn = Console.ReadLine()!.ToLowerInvariant();
                PrintNewLine();

                if (yn == "y")
                {
                    File.WriteAllText(Path.Combine(ExamplesPath, path[..^fileInfo.Extension.Length] + "__parse_out.txt"), consDesc);
                    Print("Updated output successfully.");

                    break;
                }
                else
                {
                    Print("Did not update output.");
                    PrintNewLine();
                }
            }

            break;
        }
    }

    Print("Press enter to continue.");
    Console.ReadLine();
    Clear();
}