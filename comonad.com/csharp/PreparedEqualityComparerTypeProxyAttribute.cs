using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;

namespace Digitigrade {
    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = true, Inherited = true)]
    public sealed class PreparedEqualityComparerTypeProxyAttribute : Attribute {
        private Type equalityComparerType;
        public Type EqualityComparerType { 
	    get { return equalityComparerType; } 
	}

        public PreparedEqualityComparerTypeProxyAttribute(Type equalityComparerType) {
            this.equalityComparerType = equalityComparerType;
        }
    }
}

