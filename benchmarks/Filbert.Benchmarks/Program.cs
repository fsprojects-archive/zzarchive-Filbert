using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;

using Filbert.Core;

using SimpleSpeedTester.Core;
using SimpleSpeedTester.Core.OutcomeFilters;
using SimpleSpeedTester.Interfaces;

namespace Filbert.Benchmark
{
    public class Program
    {
        // test serialization and deserialization on 100k objects
        private const int ObjectsCount = 100000;

        // perform 5 runs of each test
        private const int TestRuns = 5;

        // exclude the min and max results
        private static readonly ITestOutcomeFilter OutcomeFilter = new ExcludeMinAndMaxTestOutcomeFilter();

        private static readonly List<SimpleObject> SimpleObjects = Enumerable.Range(1, ObjectsCount).Select(GetSimpleObject).ToList();
        private static readonly List<Bert> BertSimpleObjects = Enumerable.Range(1, ObjectsCount).Select(GetSimpleObjectBert).ToList();

        private static void Main(string[] args)
        {
            // speed test protobuf-net
            DoSpeedTest(
                "Protobuf-Net",
                SimpleObjects,
                SerializeWithProtobufNet,
                DeserializeWithProtobufNet<SimpleObject>);

            // speed test Filbert
            DoSpeedTest(
                "Filbert",
                BertSimpleObjects,
                SerializeWithFilbert,
                DeserializeWithFilbert);
        }

        private static Bert GetSimpleObjectBert(int id)
        {
            return Bert.NewTuple(new[]
            {
                Bert.NewTuple(new[] { Bert.NewAtom("Name"), Bert.NewAtom("Simple") }),
                Bert.NewTuple(new[] { Bert.NewAtom("Id"), Bert.NewInteger(100000) }),
                Bert.NewTuple(new[] { Bert.NewAtom("Address"), Bert.NewByteList(System.Text.Encoding.ASCII.GetBytes("Planet Earth")) }),
                Bert.NewTuple(new[] { Bert.NewAtom("Scores"), Bert.NewList(Enumerable.Range(0, 10).Select(Bert.NewInteger).ToArray()) })                
            });
        }

        private static SimpleObject GetSimpleObject(int id)
        {
            return new SimpleObject
            {
                Name = "Simple",
                Id = 100000,
                Address = "Planet Earth",
                Scores = Enumerable.Range(0, 10).ToArray()
            };
        }

        private static void DoSpeedTest<T>(
            string testGroupName, List<T> objects, Func<List<T>, List<byte[]>> serializeFunc, Func<List<byte[]>, List<T>> deserializeFunc)
        {
            var byteArrays = new List<byte[]>();

            var testGroup = new TestGroup(testGroupName);

            var serializationTestSummary =
                testGroup
                    .Plan("Serialization", () => byteArrays = serializeFunc(objects), TestRuns)
                    .GetResult()
                    .GetSummary(OutcomeFilter);

            Console.WriteLine(serializationTestSummary);

            Console.WriteLine("Test Group [{0}] average serialized byte array size is [{1}]", testGroupName, byteArrays.Average(arr => arr.Length));

            var clones = new List<T>();

            if (deserializeFunc != null)
            {
                var deserializationTestSummary =
                    testGroup
                        .Plan("Deserialization", () => clones = deserializeFunc(byteArrays), TestRuns)
                        .GetResult()
                        .GetSummary(OutcomeFilter);

                Console.WriteLine(deserializationTestSummary);
            }

            Console.WriteLine("--------------------------------------------------------");
        }

        #region Protobuf-Net

        private static List<byte[]> SerializeWithProtobufNet<T>(List<T> objects)
        {
            return objects.Select(SerializeWithProtobufNet).ToList();
        }

        private static byte[] SerializeWithProtobufNet<T>(T obj)
        {
            using (var memStream = new MemoryStream())
            {
                ProtoBuf.Serializer.Serialize(memStream, obj);
                return memStream.ToArray();
            }
        }

        private static List<T> DeserializeWithProtobufNet<T>(List<byte[]> byteArrays)
        {
            return byteArrays.Select(DeserializeWithProtobufNet<T>).ToList();
        }

        private static T DeserializeWithProtobufNet<T>(byte[] byteArray)
        {
            using (var memStream = new MemoryStream(byteArray))
            {
                return ProtoBuf.Serializer.Deserialize<T>(memStream);
            }
        }

        #endregion

        #region Filbert

        private static List<byte[]> SerializeWithFilbert(List<Bert> objects)
        {
            return objects.Select(SerializeWithFilbert).ToList();
        }

        private static byte[] SerializeWithFilbert(Bert bert)
        {
            using (var memStream = new MemoryStream())
            {
                Filbert.Encoder.encode(bert, memStream);
                return memStream.ToArray();
            }
        }

        private static List<Bert> DeserializeWithFilbert(List<byte[]> byteArrays)
        {
            return byteArrays.Select(DeserializeWithFilbert).ToList();
        }

        private static Bert DeserializeWithFilbert(byte[] byteArray)
        {
            using (var memStream = new MemoryStream(byteArray))
            {
                return Filbert.Decoder.decode(memStream);
            }
        }

        #endregion

        [Serializable]
        [DataContract]
        public class SimpleObject
        {
            [DataMember(Order = 1)]
            public int Id { get; set; }

            [DataMember(Order = 2)]
            public string Name { get; set; }

            [DataMember(Order = 3)]
            public string Address { get; set; }

            [DataMember(Order = 4)]
            public int[] Scores { get; set; }
        }
    }
}
