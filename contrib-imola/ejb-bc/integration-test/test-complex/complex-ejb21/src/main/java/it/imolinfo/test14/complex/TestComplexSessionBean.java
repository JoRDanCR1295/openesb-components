package it.imolinfo.test14.complex;

import javax.ejb.*;
import javax.transaction.Status;

/**
 * This is the bean class for the TestComplexSessionBean enterprise bean.
 * Created 21/05/2007 14:41:22
 * @author marco
 */
public class TestComplexSessionBean implements SessionBean, TestComplexSessionRemoteBusiness {
    private SessionContext context;
    
    // <editor-fold defaultstate="collapsed" desc="EJB infrastructure methods. Click the + sign on the left to edit the code.">
    // TODO Add code to acquire and use other enterprise resources (DataSource, JMS, enterprise bean, Web services)
    // TODO Add business methods or web service operations
    /**
     * @see javax.ejb.SessionBean#setSessionContext(javax.ejb.SessionContext)
     */
    public void setSessionContext(SessionContext aContext) {
        context = aContext;
    }
    
    /**
     * @see javax.ejb.SessionBean#ejbActivate()
     */
    public void ejbActivate() {
        
    }
    
    /**
     * @see javax.ejb.SessionBean#ejbPassivate()
     */
    public void ejbPassivate() {
        
    }
    
    /**
     * @see javax.ejb.SessionBean#ejbRemove()
     */
    public void ejbRemove() {
        
    }
    // </editor-fold>
    
    /**
     * See section 7.10.3 of the EJB 2.0 specification
     * See section 7.11.3 of the EJB 2.1 specification
     */
    public void ejbCreate() {
        // TODO implement ejbCreate if necessary, acquire resources
        // This method has access to the JNDI context so resource aquisition
        // spanning all methods can be performed here such as home interfaces
        // and data sources.
    }
    
    
    
    // Add business logic below. (Right-click in editor and choose
    // "EJB Methods > Add Business Method" or "Web Service > Add Operation")
    public UserProfile getUserProfile(String code) {
        
        java.lang.System.out.println("getUserProfile called with code: " + code);
        if ("null".equals(code)) {
            return null;
        }
        if ("nullName".equals(code)) {
            return new it.imolinfo.test14.complex.UserProfile(null, "via selice 66", 31);    
        }
        if ("nullAddress".equals(code)) {
            return new it.imolinfo.test14.complex.UserProfile("marco", null, 31);    
        }
        
        it.imolinfo.test14.complex.UserProfile profile = new it.imolinfo.test14.complex.UserProfile("marco", "via selice 66", 31);
            
        return profile;
    }

    public void throwException() throws UserProfileException {
        java.lang.System.out.println("throwException called, throwsing a UserProfileException");
        throw new UserProfileException("01","test");
    }
    
      /**
   *  Return a string representation of the given status code.
   */
  private String toString(int status) {
    switch (status) {
      case javax.transaction.Status.STATUS_PREPARING:
        return "STATUS_PREPARING";
      case javax.transaction.Status.STATUS_PREPARED:
        return "STATUS_PREPARED";
      case javax.transaction.Status.STATUS_ROLLING_BACK:
        return "STATUS_ROLLING_BACK";
      case javax.transaction.Status.STATUS_ROLLEDBACK:
        return "STATUS_ROLLEDBACK";
      case javax.transaction.Status.STATUS_COMMITTING:
        return "STATUS_COMMITING";
      case javax.transaction.Status.STATUS_COMMITTED:
        return "STATUS_COMMITED";
      case javax.transaction.Status.STATUS_NO_TRANSACTION:
        return "STATUS_NO_TRANSACTION";
      case javax.transaction.Status.STATUS_UNKNOWN:
        return "STATUS_UNKNOWN";
      case javax.transaction.Status.STATUS_MARKED_ROLLBACK:
        return "STATUS_MARKED_ROLLBACK";
      case javax.transaction.Status.STATUS_ACTIVE:
        return "STATUS_ACTIVE";
      default:
        return "STATUS_UNKNOWN(" + status + ")";
    }
  }
    
    
    
    
}
