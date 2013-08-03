using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;

namespace Digitigrade {
    /// <summary>
    /// The default EqualityComparer<T> instance can be grossly inefficient as it requires the use of reflection
    /// during each comparison. This allows classes to specify their own, yielding a constant factor of ~100 
    /// performance increase if the comparer is used repeatedly. This is especially useful if the default equality
    /// comparer has to chain the use of other default equality comparers.
    /// Just use PreparedEqualityComparer<T>.Default instead of EqualityComparer<T>.Default.
    /// This gracefully degrades to the standard behavior.
    /// </summary>
    public static class PreparedEqualityComparer<T> {
        public static IEqualityComparer<T> Default {
            get {
                Type valueType = typeof(T);
                foreach (Type proxy in valueType.GetCustomAttributes(typeof(PreparedEqualityComparerTypeProxyAttribute), true).Cast<PreparedEqualityComparerTypeProxyAttribute>().Select(c => c.EqualityComparerType)) {
                    Type specializedComparer = proxy.ContainsGenericParameters
                        ? proxy.MakeGenericType(valueType.GetGenericArguments())
                        : proxy;

                    if (!specializedComparer.IsClass && !specializedComparer.IsValueType)
                        throw new ArgumentException("prepared equality comparer is not a class");
                    if (!specializedComparer.IsSubclassOf(typeof(IEqualityComparer<T>)))
                        continue;

                    ConstructorInfo constructor = specializedComparer.GetConstructor(new Type[] { });
                    if (constructor == null)
                        continue;
                    return (IEqualityComparer<T>)constructor.Invoke(new object[] { });
                }
                return EqualityComparer<T>.Default;
            }
        }
    }
}
