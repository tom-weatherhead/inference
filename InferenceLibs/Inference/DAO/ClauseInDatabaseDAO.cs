using System;
using System.Collections.Generic;
//using System.Linq;
//using System.Text;
using NHibernateHelperLib.DAO;
using Inference.Domain;

namespace Inference.DAO
{
    public class ClauseInDatabaseDAO : DAO<ClauseInDatabase>
    {
        public ClauseInDatabaseDAO()
            //: base()
        {
        }

        public override void MakePersistent(ClauseInDatabase clause)
        {
            clause.Validate();  // Throw an appropriate exception if the ClauseInDatabase object is not valid.
            base.MakePersistent(clause);
        }

        /*
        public override void MakeTransient(ClauseInDatabase clause)
        {
            // Remove books
            BookDAO bookDAO = new BookDAO();

            foreach (Book book in author.Books)
            {
                book.Authors.Remove(author);
                bookDAO.MakePersistent(book);
            }

            author.Books.Clear();

            base.MakeTransient(author);
        }
         */
    }
}
