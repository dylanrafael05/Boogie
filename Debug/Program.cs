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
".Trim());
    Print("");

    var command = Input("Enter command:");
    Clear();

    switch (command)
    {
        case "test-parser" or "0":
        {
            string[] testSources = new[]
    {
@"
b := 0
a := b + 1

if a == b do
    print! a
else
    print! b
end
"
    };

            foreach (string source in testSources)
            {
                Print(":: SOURCE ::");
                Print(source);

                Print(":: PARSED ::");
                Print(new Parser(new Lexer(source, "source.bgy")).Parse().GetDescription());
            }

            break;
        }

        case "update-test-data" or "1":
        {
            while (true)
            {
                Print("Available examples:");
                foreach(var f in Directory.EnumerateFiles(ExamplesPath))
                {
                    var fInfo = new FileInfo(f);

                    if (fInfo.Extension is ".bge" or ".boogie")
                    {
                        Print("  - " + fInfo.Name);
                    }
                }
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
                var fileInfo = new FileInfo(file);

                var cons = new Parser(new Lexer(File.ReadAllText(path), path)).Parse();
                var consDesc = cons.GetDescription();

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