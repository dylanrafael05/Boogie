using System;
using System.Collections.Generic;

namespace Boogie.Model;

public interface IForkable
{
    public void BeginFork();
    public void RollbackFork();
    public void FinalizeFork();
}
public interface IDataStream<T> : IDisposable
{
    public T Get(int offset = 0);
    public void Advance(int count = 1);
}
public interface IBuffer<T>
{
    public void Put(T value);
    public int CurrentCount { get; }
}
public interface IIterator<T>
{
    public T Current { get; }
    public void Update();
}

public class ForkableDataStream<T> : IDataStream<T>, IForkable
{
    private readonly IEnumerator<T> innerEnumerator;
    private readonly List<T> storedValues;
    private readonly Stack<int> forkStack;

    private int currentIndex;

    public T OnDry { get; private set; } 

    public ForkableDataStream(IEnumerator<T> innerEnumerator, T onDry)
    {
        this.innerEnumerator = innerEnumerator;

        storedValues = new List<T>();
        forkStack = new Stack<int>();

        currentIndex = 0;

        OnDry = onDry;
    }
    public ForkableDataStream(IEnumerable<T> innerEnumerable, T onDry) : this(innerEnumerable.GetEnumerator(), onDry)
    {}

    public T Get(int offset = 0)
    {
        var ridx = currentIndex + offset;

        while (storedValues.Count <= ridx)
        {
            if (!innerEnumerator.MoveNext())
            {
                storedValues.Add(OnDry);
            }
            else
            {
                storedValues.Add(innerEnumerator.Current);
            }
        }

        return storedValues[ridx];
    }
    public void Advance(int count = 1)
    {
        if (forkStack.Count != 0)
        {
            currentIndex += count;
        }
        else
        {
            var toRemove = Math.Min(count, storedValues.Count);
            var toAdvance = count - toRemove;

            for (int i = 0; i < toRemove; i++)
            {
                storedValues.RemoveAt(storedValues.Count - 1);
            }

            for (int i = 0; i < toAdvance; i++)
            {
                innerEnumerator.MoveNext();
            }

            //Console.WriteLine(toRemove);
            //Console.WriteLine(currentIndex - toAdvance);

            currentIndex -= toAdvance;
        }
    }

    public void BeginFork()
    {
        forkStack.Push(currentIndex);
    }

    public void RollbackFork()
    {
        currentIndex = forkStack.Pop();
    }

    public void FinalizeFork()
    {
        forkStack.Pop();
        if (forkStack.Count == 0)
        {
            currentIndex = 0;
            storedValues.Clear();
        }
    }

    public T Read(int offset = 0)
    {
        var t = Get(offset);
        Advance();
        return t;
    }

    public void Dispose()
    {
        innerEnumerator.Dispose();
    }
}
public class ForkableBuffer<T> : IBuffer<T>, IForkable
{
    private readonly Stack<T> innerStack;
    private readonly Stack<int> forkStack;

    public int CurrentCount => innerStack.Count;

    public ForkableBuffer()
    {
        innerStack = new Stack<T>();
        forkStack = new Stack<int>();
    }

    public void Put(T value)
    {
        innerStack.Push(value);
    }

    public void Remove()
    {
        innerStack.Pop();
    }

    public void RemoveMany(int count)
    {
        while (count > 0)
        {
            innerStack.Pop();
            count--;
        }
    }

    public void BeginFork()
    {
        forkStack.Push(innerStack.Count);
    }

    public void RollbackFork()
    {
        var trailoffPos = forkStack.Pop();
        RemoveMany(innerStack.Count - trailoffPos);
    }

    public void FinalizeFork()
    {
        forkStack.Pop();
    }
}
public class ForkableIterator<T> : IIterator<T>, IForkable
{
    public Stack<T> forkStack;
    private Func<T, T> updater;
    public T Current { get; private set; }

    public ForkableIterator(Func<T, T> updater, T startingValue)
    {
        this.updater = updater;
        Current = startingValue;

        forkStack = new Stack<T>();
    }

    public void Update()
    {
        Current = updater(Current);
    }

    public void BeginFork()
    {
        forkStack.Push(Current);
    }
    public void RollbackFork()
    {
        Current = forkStack.Pop();
    }
    public void FinalizeFork()
    {
        forkStack.Pop();
    }
}

public class ForkableManager : IForkable
{
    private IForkable[] managers;

    public ForkableManager(params IForkable[] managers)
    {
        this.managers = managers;
    }

    public void BeginFork()
    {
        foreach(var m in managers)
        {
            m.BeginFork();
        }
    }

    public void FinalizeFork()
    {
        foreach (var m in managers)
        {
            m.FinalizeFork();
        }
    }

    public void RollbackFork()
    {
        foreach (var m in managers)
        {
            m.RollbackFork();
        }
    }
}