using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using Microsoft.FSharp.Collections;

namespace AdventOfCode.Runner
{
    public class Program
    {
        public static void Main(string[] args)
        {
            const int solved = 1;

            string header = "Advent of Code 2019";
            string separator = new string('=', header.Length);

            Console.WriteLine(header);
            Console.WriteLine(separator);
            Console.WriteLine();

            //int day = args.Length > 0 ? ParseDay(args[0], 0, solved) : 0;

            //if (day == 0)
            //{
            //    Console.Write("Which day you would like to run? ");
                
            //    string input = Console.ReadLine();
            //    day = ParseDay(input, 0, solved);
            //}

            //if (day == 0)
            //    return;

            var v = AdventOfCode.Intcode.Computer.execute;

            Console.WriteLine();

            if (Debugger.IsAttached)
                return;

            Console.WriteLine("Press any key to exit...");
            Console.ReadKey();
        }

        private static int ParseDay(string input, int lower, int upper)
        {
            if (!int.TryParse(input, out int day))
                return 0;

            if (day <= lower || day > upper)
                return 0;

            return day;
        }
    }
}
