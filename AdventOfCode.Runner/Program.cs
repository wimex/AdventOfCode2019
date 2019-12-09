using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using AdventOfCode.Shared;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Reflection;

namespace AdventOfCode.Runner
{
    public class Program
    {
        public static void Main(string[] args)
        {
            List<Type> solutions = new List<Type>
            {
                typeof(AdventOfCode.Day01.Puzzles), typeof(AdventOfCode.Day02.Puzzles), typeof(AdventOfCode.Day03.Puzzles),
                typeof(AdventOfCode.Day04.Puzzles), typeof(AdventOfCode.Day05.Puzzles), typeof(AdventOfCode.Day06.Puzzles),
                typeof(AdventOfCode.Day07.Puzzles), typeof(AdventOfCode.Day08.Puzzles)
            };
        
            string header = "Advent of Code 2019";
            string separator = new string('=', header.Length);

            Console.WriteLine(header);
            Console.WriteLine(separator);
            Console.WriteLine();

            Console.Write("Which day you would like to run (or type 0 to exit)? ");

            string input = Console.ReadLine();
            if (input == "0")
                return;

            if (!ParseDay(input, 1, solutions.Count, out int day))
                return;

            string ident = day.ToString().PadLeft(2, '0');
            string filename = $"App_Data{Path.DirectorySeparatorChar}day{ident}.txt";
            string library = $"AdventOfCode.Day{ident}.Puzzles";

            Type type = solutions[day - 1];
            MethodInfo method = type.GetMethod("puzzles");
            if (method == null)
            {
                Console.WriteLine("Unable to locate entry method");
                return;
            }

            Console.WriteLine();
            method.Invoke(null, new object[] {filename});

            Console.WriteLine();

            if (Debugger.IsAttached)
                return;

            Console.WriteLine("Press any key to exit...");
            Console.ReadKey();
        }

        private static bool ParseDay(string input, int lower, int upper, out int day)
        {
            if (!int.TryParse(input, out int temp))
            {
                day = 0;
                return false;
            }

            if (temp < lower || temp > upper)
            {
                day = 0;
                return false;
            }

            day = temp;
            return true;
        }
    }
}
