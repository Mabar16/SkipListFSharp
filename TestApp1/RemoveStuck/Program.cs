using System;
using DataStructures;
using DataStructures.Lists;

namespace RemoveStuck
{
    class Program
    {
        static void Main(string[] args)
        {

            
            var list = new SkipList<int>(3);

            list.Add(-2);
            list.Add(0);
            Console.WriteLine("stuck? " + list._currentMaxLevel);
            list.Remove(0);
            list.Remove(0);
            foreach (var thing in list)
            {
                Console.WriteLine("Im not stuck at all ;)");
            }
            Console.WriteLine("no stuck");

        }
    }
}
