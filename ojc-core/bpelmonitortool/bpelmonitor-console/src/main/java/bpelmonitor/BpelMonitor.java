/*
 * BpelMonitor1.java
 *
 * Created on May 7, 2009, 1:32:53 PM
 * Copyright mbhasin
 */
package bpelmonitor;

import bpelmonitor.model.DashboardEntry;
import bpelmonitor.jbiruntime.BPELSERuntime;
import bpelmonitor.jbiruntime.JBIRuntime;
import bpelmonitor.model.ServiceAssembly;
import bpelmonitor.model.ServiceUnit;
import com.icesoft.faces.component.ext.RowSelectorEvent;
import com.icesoft.faces.component.jsfcl.data.DefaultTableDataModel;
import com.icesoft.faces.component.tree.IceUserObject;
import com.sun.rave.web.ui.appbase.AbstractPageBean;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.faces.FacesException;
import javax.faces.context.FacesContext;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.xml.namespace.QName;

/**
 * <p>Page bean that corresponds to a similarly named JSP page.  This
 * class contains component definitions (and initialization code) for
 * all components that you have defined on this page, as well as
 * lifecycle methods and event handlers where you may add behavior
 * to respond to incoming events.</p>
 */
public class BpelMonitor extends AbstractPageBean {
    // <editor-fold defaultstate="collapsed" desc="Managed Component Definition">

    private int __placeholder;

    /**
     * <p>Automatically managed component initialization.  <strong>WARNING:</strong>
     * This method is automatically generated, so any user-specified code inserted
     * here is subject to being replaced.</p>
     */
    private void _init() throws Exception {
    }
    private DefaultTableDataModel dataTable1Model = new DefaultTableDataModel();

    public DefaultTableDataModel getDataTable1Model() {
        return dataTable1Model;
    }

    public void setDataTable1Model(DefaultTableDataModel dtdm) {
        this.dataTable1Model = dtdm;
    }

    // </editor-fold>
    /**
     * <p>Construct a new Page bean instance.</p>
     */
    public BpelMonitor() {
    }

    /**
     * <p>Callback method that is called whenever a page is navigated to,
     * either directly via a URL, or indirectly via page navigation.
     * Customize this method to acquire resources that will be needed
     * for event handlers and lifecycle methods, whether or not this
     * page is performing post back processing.</p>
     * 
     * <p>Note that, if the current request is a postback, the property
     * values of the components do <strong>not</strong> represent any
     * values submitted with this request.  Instead, they represent the
     * property values that were saved for this view when it was rendered.</p>
     */
    @Override
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
            log("BpelMonitor Initialization Failure", e);
            throw e instanceof FacesException ? (FacesException) e : new FacesException(e);
        }

    // </editor-fold>
    // Perform application initialization that must complete
    // *after* managed components are initialized
    // TODO - add your own initialization code here
    }

    /**
     * <p>Callback method that is called after the component tree has been
     * restored, but before any event processing takes place.  This method
     * will <strong>only</strong> be called on a postback request that
     * is processing a form submit.  Customize this method to allocate
     * resources that will be required in your event handlers.</p>
     */
    @Override
    public void preprocess() {
    }

    /**
     * <p>Callback method that is called just before rendering takes place.
     * This method will <strong>only</strong> be called for the page that
     * will actually be rendered (and not, for example, on a page that
     * handled a postback and then navigated to a different page).  Customize
     * this method to allocate resources that will be required for rendering
     * this page.</p>
     */
    @Override
    public void prerender() {
        getSessionBean1().setBpTreeModel(getBpTreeModel());
        getDashboardData();
    }

    /**
     * <p>Callback method that is called after rendering is completed for
     * this request, if <code>init()</code> was called (regardless of whether
     * or not this was the page that was actually rendered).  Customize this
     * method to release resources acquired in the <code>init()</code>,
     * <code>preprocess()</code>, or <code>prerender()</code> methods (or
     * acquired during execution of an event handler).</p>
     */
    @Override
    public void destroy() {
    }

    /**
     * <p>Return a reference to the scoped data bean.</p>
     *
     * @return reference to the scoped data bean
     */
    protected SessionBean1 getSessionBean1() {
        return (SessionBean1) getBean("SessionBean1");
    }

    /**
     * <p>Return a reference to the scoped data bean.</p>
     *
     * @return reference to the scoped data bean
     */
    protected RequestBean1 getRequestBean1() {
        return (RequestBean1) getBean("RequestBean1");
    }

    /**
     * <p>Return a reference to the scoped data bean.</p>
     *
     * @return reference to the scoped data bean
     */
    protected ApplicationBean1 getApplicationBean1() {
        return (ApplicationBean1) getBean("ApplicationBean1");
    }

    private DefaultTreeModel getBpTreeModel() {
        DefaultMutableTreeNode rootTreeNode = new DefaultMutableTreeNode();
        IceUserObject rootObject = new IceUserObject(rootTreeNode);
        rootObject.setText("JBI Runtime");
        rootObject.setExpanded(true);
        rootTreeNode.setUserObject(rootObject);

        // bpTreeModel is accessed by by the ice:tree component
        DefaultTreeModel bpTreeModel = new DefaultTreeModel(rootTreeNode);

        List<ServiceAssembly> serviceAssemblies = new JBIRuntime().getServiceAssemblies();
        addServiceAssemblyNodes(rootTreeNode, serviceAssemblies);
        return bpTreeModel;
    }

    private void addServiceAssemblyNodes(DefaultMutableTreeNode rootTreeNode, List<ServiceAssembly> nodes) {
        ArrayList<DashboardEntry> dashboardEntries = new ArrayList();
        DashboardEntry dashBoardEntry = null;

        for (Iterator<ServiceAssembly> saIter = nodes.iterator(); saIter.hasNext();) {
            String saName = saIter.next().getName();

            DefaultMutableTreeNode saNode = addNode(rootTreeNode, saName, false);
            List<ServiceUnit> sus = new JBIRuntime().getServiceUnitsForServiceAssembly(saName);

            for (Iterator<ServiceUnit> suIter = sus.iterator(); suIter.hasNext();) {
                String suName = suIter.next().getName();
                String suDisplayName = suName.substring(saName.length() + 1);
                DefaultMutableTreeNode suNode = addNode(saNode, suDisplayName, false);
                //for each service unit, create and add nodes for the business processes
                List<String> bps = getApplicationBean1().getBPELSERuntime().getBusinessProcesses(suName);

                if (bps == null) {
                    Logger.getLogger(BPELSERuntime.class.getName()).log(Level.INFO, null, "No Business Process returned by BPELSE for " + suName);
                    return;
                }

                for (Iterator<String> bpIter = bps.iterator(); bpIter.hasNext();) {
                    String bpQNameAsString = bpIter.next();
                    QName bpQName = QName.valueOf(bpQNameAsString);

                    dashBoardEntry = new DashboardEntry();
                    dashBoardEntry.setApplicaitonName(saName);
                    dashBoardEntry.setBusinessProcessName(bpQName.toString());
                    dashBoardEntry.setBusinessProcessQName(bpQName);
                    dashboardEntries.add(dashBoardEntry);

                    addNode(suNode, bpQName.getLocalPart() + ".bpel", true);
                }
            }
        }
        getSessionBean1().setDashboardEntries(dashboardEntries);
    }

    private DefaultMutableTreeNode addNode(DefaultMutableTreeNode treeNode, String name, boolean isLeafNode) {
        DefaultMutableTreeNode branchNode = new DefaultMutableTreeNode();
        IceUserObject branchObject = new IceUserObject(branchNode);
        branchObject.setText(name);
        branchNode.setUserObject(branchObject);
        branchObject.setLeaf(isLeafNode);
        treeNode.add(branchNode);
        return branchNode;
    }

    private void getDashboardData() {
        ArrayList<DashboardEntry> dashboardEntries = getSessionBean1().getDashboardEntries();
        ArrayList<DashboardEntry> entries = getEntries(dashboardEntries);
        getDataTable1Model().setWrappedData(entries);
    }

    private ArrayList<DashboardEntry> getEntries(ArrayList<DashboardEntry> entries1) {

        ArrayList<DashboardEntry> dashboardEntries = getApplicationBean1().getBPELSERuntime().getDashboardData(entries1);
        DashboardEntry entryTO = null;
        DashboardEntry entry = null;

        ArrayList<DashboardEntry> dashboardData = new ArrayList();
        if (dashboardEntries != null && dashboardEntries.size() > 0) {
            for (Iterator<DashboardEntry> iter = dashboardEntries.iterator(); iter.hasNext();) {
                entryTO = iter.next();
                entry = new DashboardEntry();
                entry.setApplicaitonName(entryTO.getApplicaitonName());
                entry.setBusinessProcessName((QName.valueOf(entryTO.getBusinessProcessName())).getLocalPart() + ".bpel");
                entry.setLastInstanceProcessedTime(entryTO.getLastInstanceProcessedTime());
                entry.setLiveInstnces(entryTO.getLiveInstnces());
                entry.setSuspendedInstances(entryTO.getSuspendedInstances());
                entry.setFaultedInstances(entryTO.getFaultedInstances());
                entry.setTerminatedInstances(entryTO.getTerminatedInstances());
                entry.setKpi(entryTO.getKpi());
                dashboardData.add(entry);
            }
        }

        return dashboardData;
    }

    /*selection listener is used when a row is selected on the data table component
     *@param e RowSelectorEvent */
    public void rowSelector1_processAction(RowSelectorEvent rse) {
        int selectedRowIndex = rse.getRow();
        ArrayList<DashboardEntry> dashboardEntries = getSessionBean1().getDashboardEntries();
        DashboardEntry selectedEntry = dashboardEntries.get(selectedRowIndex);
        getSessionBean1().setSelectedProcess(selectedEntry);
        FacesContext facesContext = FacesContext.getCurrentInstance();
        getApplication().getNavigationHandler().handleNavigation(facesContext, null, "processMonitor");
    }

    public String dataTable1_firstPageAction() {
        {
            return null;
        }    }

    public String dataTable1_previousPageAction() {
        {
            return null;
        }    }

    public String dataTable1_nextPageAction() {
        {
            return null;
        }    }

    public String dataTable1_lastPageAction() {
        {
            return null;
        }    }
}

