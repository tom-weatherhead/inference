using System;
//using System.Collections.Generic;
//using System.Linq;
//using System.Text;
using System.Web;

namespace NHibernateHelperLib.Persistence
{
    public class HttpRequestModule : IHttpModule
    {
        public String ModuleName
        {
            get
            {
                return "NHibernateHelper";
            }
        }

        public void Init(HttpApplication application)
        {
            application.BeginRequest += new EventHandler(this.Application_BeginRequest);
            application.EndRequest += new EventHandler(this.Application_EndRequest);
            application.Error += new EventHandler(this.Application_Error);
        }

        private void Application_BeginRequest(object source, EventArgs e)
        {
            // Note: This is useless (because the session will be eventually created when needed)
            // And it may be a waste of resources because it might not be used at all
            //NHibernateHelper.GetSession();   // TW 2011/07/23 : Commented out.
        }

        private void Application_EndRequest(object source, EventArgs e)
        {
            NHibernateHelper.CloseSession();
        }

        private void Application_Error(object sender, EventArgs e)
        {
            // TODO: Useful to add?
            // Most of the time, it should be possible to catch the error and rollback
            NHibernateHelper.RollbackTransaction();    // This also closes the Session.
        }

        public void Dispose()
        {
        }
    }
}
