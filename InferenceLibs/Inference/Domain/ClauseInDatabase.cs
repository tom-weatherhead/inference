using System;
using System.Collections.Generic;
//using System.Linq;
using System.Text;

namespace Inference.Domain
{
    [Serializable]
    public class ClauseInDatabase //: IDomainObject
    {
        public virtual Guid Id { get; set; }
        public virtual int Version { get; set; }
        public virtual string Clause { get; set; }

        public ClauseInDatabase()
        {
            Id = Guid.Empty;
            Version = 0;
            Clause = string.Empty;
        }

        public ClauseInDatabase(string clause)
            : this()
        {

            if (string.IsNullOrEmpty(clause))
            {
                throw new ArgumentNullException("clause", "ClauseInDatabase constructor error: clause is null or empty.");
            }

            Clause = clause;
        }

        public override bool Equals(object obj)
        {

            if (object.ReferenceEquals(this, obj))
            {
                return true;
            }

            ClauseInDatabase otherClauseInDatabase = obj as ClauseInDatabase;

            if (otherClauseInDatabase == null)
            {
                return false;
            }

            return Clause == otherClauseInDatabase.Clause;
        }

        public override int GetHashCode()
        {
            return 101 * Clause.GetHashCode() + 97;
        }

        public virtual void Validate()
        {

            if (string.IsNullOrEmpty(Clause))
            {
                throw new Exception("ClauseInDatabase validation error: Clause is null or empty.");
            }
        }
    }
}
