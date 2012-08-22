using System;
using System.Collections.Generic;
using System.IO;
using Filbert.Core;
using Filbert.Rpc;

namespace Filbert.ExampleCs
{
    /// <summary>
    /// Test app for Filbert, in CSharp
    /// </summary>
    public class Program
    {
        /// <summary>
        /// Encode a bert and then decode it
        /// </summary>
        public static void Main()
        {
            // create a new BERT
            var mysterWord = new byte[] { 131, 107, 0, 8, 104, 97, 122, 101, 108, 110, 117, 116 };
            var dict = new Dictionary<Bert, Bert>
            {
                { Bert.NewAtom("Filbert"), Bert.NewAtom("means") },
                { Bert.NewByteList(mysterWord), Bert.NewAtom("!") }
            };
            var bert = Bert.FromDict(dict);

            using (var memStream = new MemoryStream())
            {
                // encode the BERT
                Filbert.Encoder.encode(bert, memStream);
                Console.WriteLine("{0} bytes encoded...", memStream.Length);

                memStream.Position = 0;
                var bertClone = Filbert.Decoder.decode(memStream);

                if (bert.Equals(bertClone))
                {
                    Console.WriteLine("decoded successfully, they're a match!");
                }
                else
                {
                    Console.WriteLine("back to work YC *cracks whip*");
                }
            }

            Console.WriteLine("Making synchronous RPC call");

            var client = BertRpcClient.Start("localhost", 9997);
            var result = client.CallAsTask("nat", "add", Bert.NewInteger(1), Bert.NewInteger(100))
                               .Result;

            Console.WriteLine("Got {0}", result);

            Console.WriteLine("Making asynchronous RPC call");

            client.CastAsTask("nat", "die", Bert.NewInteger(666)).Wait();

            Console.WriteLine("Press any key to exit..");
            Console.ReadKey();
        }
    }
}
