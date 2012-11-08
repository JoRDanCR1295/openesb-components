/*
 * SessionBean1.java
 *
 * Created on Apr 27, 2009, 6:21:47 PM
 * Copyright mbhasin
 */
package bpelmonitor;

import bpelmonitor.model.DashboardEntry;
import bpelmonitor.model.InstanceFilter;
import com.sun.rave.web.ui.appbase.AbstractSessionBean;
import com.sun.sql.rowset.CachedRowSetXImpl;
import java.util.ArrayList;
import javax.faces.FacesException;
import javax.swing.tree.DefaultTreeModel;

/**
 * <p>Session scope data bean for your application.  Create properties
 *  here to represent cached data that should be made available across
 *  multiple HTTP requests for an individual user.</p>
 *
 * <p>An instance of this class will be created for you automatically,
 * the first time your application evaluates a value binding expression
 * or method binding expression that references a managed bean using
 * this class.</p>
 */
public class SessionBean1 extends AbstractSessionBean {
    // <editor-fold defaultstate="collapsed" desc="Managed Component Definition">

    private int __placeholder;
    private DashboardEntry selectedProcess;
    private ArrayList<DashboardEntry> dashboardEntries = new ArrayList();
    private DefaultTreeModel bpTreeModel = null;
    private InstanceFilter instanceFilter = null;


    /**
     * <p>Automatically managed component initialization.  <strong>WARNING:</strong>
     * This method is automatically generated, so any user-specified code inserted
     * here is subject to being replaced.</p>
     */
    private void _init() throws Exception {
        monitorbpelinstanceRowSet.setDataSourceName("java:comp/env/jdbc/USR2_ApacheDerby");
        monitorbpelinstanceRowSet.setCommand("SELECT ALL 'false' as SELECTED, USR2.MONITORBPELINSTANCE.ENGINEID, USR2.MONITORBPELINSTANCE.INSTANCEID, USR2.MONITORBPELINSTANCE.BPELID, USR2.MONITORBPELINSTANCE.STATUS, USR2.MONITORBPELINSTANCE.STARTTIME, USR2.MONITORBPELINSTANCE.ENDTIME, USR2.MONITORBPELINSTANCE.UPDATEDTIME  FROM USR2.MONITORBPELINSTANCE WHERE USR2.MONITORBPELINSTANCE.BPELID = ?          AND USR2.MONITORBPELINSTANCE.STATUS  LIKE  ? ORDER BY USR2.MONITORBPELINSTANCE.BPELID ASC,                     USR2.MONITORBPELINSTANCE.STATUS ASC");
        monitorbpelinstanceRowSet.setTableName("MONITORBPELINSTANCE");
        monitorbpelvariableRowSet.setDataSourceName("java:comp/env/jdbc/USR2_ApacheDerby");
        monitorbpelvariableRowSet.setCommand("SELECT ALL USR2.MONITORBPELVARIABLE.INSTANCEID, USR2.MONITORBPELVARIABLE.SCOPEID, USR2.MONITORBPELVARIABLE.VARID, USR2.MONITORBPELVARIABLE.VARNAME, USR2.MONITORBPELVARIABLE.ISFAULT, USR2.MONITORBPELVARIABLE.VARVALUE  FROM USR2.MONITORBPELVARIABLE WHERE USR2.MONITORBPELVARIABLE.INSTANCEID = ?");
        monitorbpelvariableRowSet.setTableName("MONITORBPELVARIABLE");
    }
    private CachedRowSetXImpl monitorbpelinstanceRowSet = new CachedRowSetXImpl();

    public CachedRowSetXImpl getMonitorbpelinstanceRowSet() {
        return monitorbpelinstanceRowSet;
    }

    public void setMonitorbpelinstanceRowSet(CachedRowSetXImpl crsxi) {
        this.monitorbpelinstanceRowSet = crsxi;
    }
    private CachedRowSetXImpl monitorbpelvariableRowSet = new CachedRowSetXImpl();

    public CachedRowSetXImpl getMonitorbpelvariableRowSet() {
        return monitorbpelvariableRowSet;
    }

    public void setMonitorbpelvariableRowSet(CachedRowSetXImpl crsxi) {
        this.monitorbpelvariableRowSet = crsxi;
    }
    // </editor-fold>

    /**
     * <p>Construct a new session data bean instance.</p>
     */
    public SessionBean1() {
    }

    /**
     * <p>This method is called when this bean is initially added to
     * session scope.  Typically, this occurs as a result of evaluating
     * a value binding or method binding expression, which utilizes the
     * managed bean facility to instantiate this bean and store it into
     * session scope.</p>
     * 
     * <p>You may customize this method to initialize and cache data values
     * or resources that are required for the lifetime of a particular
     * user session.</p>
     */
    public void init() {
        // Perform initializations inherited from our superclass
        super.init();
        // Perform application initialization that must complete
        // *before* managed components are initialized
        // TODO - add your own initialiation code here

        // <editor-fold defaultstate="collapsed" desc="Managed Component Initialization">
        // Initialize automatically managed components
        // *Note* - this logic should NOT be modified
        try {
            _init();
        } catch (Exception e) {
            log("SessionBean1 Initialization Failure", e);
            throw e instanceof FacesException ? (FacesException) e : new FacesException(e);
        }

    // </editor-fold>
    // Perform application initialization that must complete
    // *after* managed components are initialized
    // TODO - add your own initialization code here
    }

    /**
     * <p>This method is called when the session containing it is about to be
     * passivated.  Typically, this occurs in a distributed servlet container
     * when the session is about to be transferred to a different
     * container instance, after which the <code>activate()</code> method
     * will be called to indicate that the transfer is complete.</p>
     * 
     * <p>You may customize this method to release references to session data
     * or resources that can not be serialized with the session itself.</p>
     */
    public void passivate() {
    }

    /**
     * <p>This method is called when the session containing it was
     * reactivated.</p>
     * 
     * <p>You may customize this method to reacquire references to session
     * data or resources that could not be serialized with the
     * session itself.</p>
     */
    public void activate() {
    }

    /**
     * <p>This method is called when this bean is removed from
     * session scope.  Typically, this occurs as a result of
     * the session timing out or being terminated by the application.</p>
     * 
     * <p>You may customize this method to clean up resources allocated
     * during the execution of the <code>init()</code> method, or
     * at any later time during the lifetime of the application.</p>
     */
    public void destroy() {
    }

    public void setSelectedProcess(DashboardEntry entry) {
        this.selectedProcess = entry;
    }

    /**
     * @return the selectedProcess
     */
    public DashboardEntry getSelectedProcess() {
        return selectedProcess;
    }

    /**
     * @return the dashboardEntries
     */
    public ArrayList<DashboardEntry> getDashboardEntries() {
        return dashboardEntries;
    }

    /**
     * @param dashboardEntries the dashboardEntries to set
     */
    public void setDashboardEntries(ArrayList<DashboardEntry> dashboardEntries) {
        this.dashboardEntries = dashboardEntries;
    }

    /**
     * @return the bpTreeModel
     */
    public DefaultTreeModel getBpTreeModel() {
        return bpTreeModel;
    }

    /**
     * @param bpTreeModel the bpTreeModel to set
     */
    public void setBpTreeModel(DefaultTreeModel bpTreeModel) {
        this.bpTreeModel = bpTreeModel;
    }

    /**
     * @return the instanceFilter
     */
    public InstanceFilter getInstanceFilter() {
        return instanceFilter;
    }

    public void setInstanceFilter(InstanceFilter filter) {
        this.instanceFilter = filter;
    }
}
