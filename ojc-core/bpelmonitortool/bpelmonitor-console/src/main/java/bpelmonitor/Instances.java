/*
 * Instances.java
 *
 * Created on May 8, 2009, 2:06:21 PM
 * Copyright mbhasin
 */
package bpelmonitor;

import bpelmonitor.model.DashboardEntry;
import bpelmonitor.model.InstanceFilter;
import bpelmonitor.model.InstanceFilter.InstanceStatus2;
import bpelmonitor.model.InstanceVariable;
import com.icesoft.faces.component.DisplayEvent;
import com.icesoft.faces.component.ext.HtmlSelectOneMenu;
import com.icesoft.faces.component.ext.RowSelector;
import com.icesoft.faces.component.ext.RowSelectorEvent;
import com.icesoft.faces.component.jsfcl.data.CachedRowSetWrapperDataModel;
import com.icesoft.faces.component.jsfcl.data.DefaultSelectedData;
import com.icesoft.faces.component.jsfcl.data.DefaultSelectionItems;
import com.icesoft.faces.component.jsfcl.data.DefaultTableDataModel;
import com.sun.data.provider.RowKey;
import com.sun.data.provider.impl.CachedRowSetDataProvider;
import com.sun.rave.faces.data.CachedRowSetDataModel;
import com.sun.rave.web.ui.appbase.AbstractPageBean;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.faces.FacesException;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import javax.faces.event.ValueChangeEvent;
import javax.faces.model.ListDataModel;
import javax.faces.model.SelectItem;
import javax.sql.rowset.serial.SerialClob;
import javax.sql.rowset.serial.SerialException;
import javax.xml.namespace.QName;

/**
 * <p>Page bean that corresponds to a similarly named JSP page.  This
 * class contains component definitions (and initialization code) for
 * all components that you have defined on this page, as well as
 * lifecycle methods and event handlers where you may add behavior
 * to respond to incoming events.</p>
 */
public class Instances extends AbstractPageBean {
    // <editor-fold defaultstate="collapsed" desc="Managed Component Definition">

    private int __placeholder;
    private String MOST_RECENT_INSTANCE = "Most Recent Instance";
    private String OLDEST_INSTANCE = "Oldest Instance";
    private String ALL_AVAILABLE_INSTANCES = "All Available Instances";
    private String ALL_AVAILABLE_STATUS = "All Available Status";
    private boolean renderSVG = true;
    private boolean showVariableDataPopup = false;

    //private ArrayList<SelectItem> bpMenuList = new ArrayList<SelectItem>();
    /**
     * <p>Automatically managed component initialization.  <strong>WARNING:</strong>
     * This method is automatically generated, so any user-specified code inserted
     * here is subject to being replaced.</p>
     */
    private void _init() throws Exception {
        monitorbpelinstanceDataProvider.setCachedRowSet((javax.sql.rowset.CachedRowSet) getValue("#{SessionBean1.monitorbpelinstanceRowSet}"));
        instancesDataTableCachedRowSetWrapperDataModel.setWrappedData(monitorbpelinstanceDataProvider.getCachedRowSet());
        monitorbpelvariableDataProvider.setCachedRowSet((javax.sql.rowset.CachedRowSet) getValue("#{SessionBean1.monitorbpelvariableRowSet}"));
    }
    private CachedRowSetDataProvider monitorbpelinstanceDataProvider = new CachedRowSetDataProvider();

    public CachedRowSetDataProvider getMonitorbpelinstanceDataProvider() {
        return monitorbpelinstanceDataProvider;
    }

    public void setMonitorbpelinstanceDataProvider(CachedRowSetDataProvider crsdp) {
        this.monitorbpelinstanceDataProvider = crsdp;
    }
    private CachedRowSetWrapperDataModel instancesDataTableCachedRowSetWrapperDataModel = new CachedRowSetWrapperDataModel();

    public CachedRowSetWrapperDataModel getDataTable1CachedRowSetWrapperDataModel() {
        return instancesDataTableCachedRowSetWrapperDataModel;
    }

    public void setDataTable1CachedRowSetWrapperDataModel(CachedRowSetWrapperDataModel crswdm) {
        this.instancesDataTableCachedRowSetWrapperDataModel = crswdm;
    }
    private CachedRowSetDataProvider monitorbpelvariableDataProvider = new CachedRowSetDataProvider();

    public CachedRowSetDataProvider getMonitorbpelvariableDataProvider() {
        return monitorbpelvariableDataProvider;
    }

    public void setMonitorbpelvariableDataProvider(CachedRowSetDataProvider crsdp) {
        this.monitorbpelvariableDataProvider = crsdp;
    }
    private CachedRowSetWrapperDataModel variablesDataTableModel = new CachedRowSetWrapperDataModel();

    public void setVariablesDataTableModel(CachedRowSetWrapperDataModel dtdm) {
        this.variablesDataTableModel = dtdm;
    }

    public CachedRowSetWrapperDataModel getVariablesDataTableModel() {
        return variablesDataTableModel;
    }
    private DefaultSelectedData selectOneMenu1Bean = new DefaultSelectedData();

    public DefaultSelectedData getSelectOneMenu1Bean() {
        return selectOneMenu1Bean;
    }

    public void setSelectOneMenu1Bean(DefaultSelectedData dsd) {
        this.selectOneMenu1Bean = dsd;
    }
    private DefaultSelectionItems selectOneMenu1DefaultItems = new DefaultSelectionItems();

    public DefaultSelectionItems getSelectOneMenu1DefaultItems() {
        return selectOneMenu1DefaultItems;
    }

    public void setSelectOneMenu1DefaultItems(DefaultSelectionItems dsi) {
        this.selectOneMenu1DefaultItems = dsi;
    }
    private HtmlSelectOneMenu selectBusinessProcess = new HtmlSelectOneMenu();

    public HtmlSelectOneMenu getSelectBusinessProcess() {
        return selectBusinessProcess;
    }

    public void setSelectBusinessProcess(HtmlSelectOneMenu hsom) {
        this.selectBusinessProcess = hsom;
    }
    private DefaultSelectedData selectOneMenu2Bean = new DefaultSelectedData();

    public DefaultSelectedData getSelectOneMenu2Bean() {
        return selectOneMenu2Bean;
    }

    public void setSelectOneMenu2Bean(DefaultSelectedData dsd) {
        this.selectOneMenu2Bean = dsd;
    }
    private DefaultSelectionItems selectOneMenu2DefaultItems = new DefaultSelectionItems();

    public DefaultSelectionItems getSelectOneMenu2DefaultItems() {
        return selectOneMenu2DefaultItems;
    }

    public void setSelectOneMenu2DefaultItems(DefaultSelectionItems dsi) {
        this.selectOneMenu2DefaultItems = dsi;
    }
    private DefaultSelectedData selectOneMenu3Bean = new DefaultSelectedData();

    public DefaultSelectedData getSelectOneMenu3Bean() {
        return selectOneMenu3Bean;
    }

    public void setSelectOneMenu3Bean(DefaultSelectedData dsd) {
        this.selectOneMenu3Bean = dsd;
    }
    private DefaultSelectionItems selectOneMenu3DefaultItems = new DefaultSelectionItems();

    public DefaultSelectionItems getSelectOneMenu3DefaultItems() {
        return selectOneMenu3DefaultItems;
    }

    public void setSelectOneMenu3DefaultItems(DefaultSelectionItems dsi) {
        this.selectOneMenu3DefaultItems = dsi;
    }


    // </editor-fold>
    /**
     * <p>Construct a new Page bean instance.</p>
     */
    public Instances() {
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
        InstanceFilter filter = getSessionBean1().getInstanceFilter();
        QName selectedBusinessProcess = filter.getBpName();

        ArrayList<DashboardEntry> entries = getSessionBean1().getDashboardEntries();
        DefaultSelectionItems items = new DefaultSelectionItems();
        items.clear();

        int i = 0, selectedInt = 0;
        for (Iterator<DashboardEntry> iter = entries.iterator(); iter.hasNext();) {
            DashboardEntry entry = iter.next();
            String bpName = entry.getBusinessProcessQName().getLocalPart();
            SelectItem item = new SelectItem(i, bpName);
            if (bpName.equals(selectedBusinessProcess.getLocalPart())) {
                selectedInt = i;
            }
            items.add(item);
            i++;
        }

        setSelectOneMenu1DefaultItems(items);
        getSelectOneMenu1Bean().setSelectedInt(selectedInt);
        getTableData();

        initializeInstanceSubFilterSelectMenus();
        initializeStatusFilterSelectMenus();
        setSubStatusFilterSelectedMenu();
        setStatusFilterSelectedMenu();
        // <editor-fold defaultstate="collapsed" desc="Managed Component Initialization">
        // Initialize automatically managed components
        // *Note* - this logic should NOT be modified
        try {
            _init();
        } catch (Exception e) {
            log("Instances Initialization Failure", e);
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
        getTableData();
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
        monitorbpelinstanceDataProvider.close();
    }

    private void getTableData() {
        InstanceFilter filter = getSessionBean1().getInstanceFilter();
        //ActivityStatus.InstanceStatus status = filter.getStatus();
        InstanceFilter.InstanceStatus2 status = filter.getStatus();
        String statusParameter = null;
        if (status == null || status.toString().equals(ALL_AVAILABLE_STATUS)) {
            statusParameter = "%";
        } else {
            statusParameter = status.toString();
        }
        try {
            getSessionBean1().getMonitorbpelinstanceRowSet().setObject(1, filter.getBpName().toString());
            getSessionBean1().getMonitorbpelinstanceRowSet().setString(2, statusParameter);
            monitorbpelinstanceDataProvider.refresh();
            instancesDataTableCachedRowSetWrapperDataModel.setWrappedData(monitorbpelinstanceDataProvider.getCachedRowSet());
        } catch (SQLException ex) {
            Logger.getLogger(Instances.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    private void initializeInstanceSubFilterSelectMenus() {
        String[] instanceFilter = {ProcessMonitor.MOST_RECENT_INSTANCE_DISPLAY,
            ProcessMonitor.OLDEST_INSTANCE_DISPLAY,
            ProcessMonitor.ALL_AVAILABLE_INSTANCES_DISPLAY};
        getSelectOneMenu2DefaultItems().clear();
        getSelectOneMenu2DefaultItems().setItems(instanceFilter);
    }

    private void initializeStatusFilterSelectMenus() {
        String[] statusFilter = {
            InstanceStatus2.RUNNING.toString(),
            InstanceStatus2.COMPLETED.toString(),
            InstanceStatus2.SUSPENDED.toString(),
            InstanceStatus2.FAULTED.toString(),
            InstanceStatus2.TERMINATED.toString(),
            ALL_AVAILABLE_STATUS};
        getSelectOneMenu3DefaultItems().clear();
        getSelectOneMenu3DefaultItems().setItems(statusFilter);
    }

    private void setSubStatusFilterSelectedMenu() {
        InstanceFilter filter = getSessionBean1().getInstanceFilter();
        getSelectOneMenu2Bean().setSelectedItem(filter.getSubFilter());
    }

    private void setStatusFilterSelectedMenu() {
        InstanceFilter filter = getSessionBean1().getInstanceFilter();
        getSelectOneMenu3Bean().setSelectedItem(filter.getStatus().toString());
    }

    /**
     * <p>Return a reference to the scoped data bean.</p>
     *
     * @return reference to the scoped data bean
     */
    protected ApplicationBean1 getApplicationBean1() {
        return (ApplicationBean1) getBean("ApplicationBean1");
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

    public void selectBusinessProcess_processValueChange(ValueChangeEvent vce) {
        Object value = vce.getNewValue();
        if (value != null && value instanceof Integer) {
            int selectedIndex = ((Integer) value).intValue();
            ArrayList<DashboardEntry> dashboardEntries = getSessionBean1().getDashboardEntries();
            DashboardEntry selectedEntry = dashboardEntries.get(selectedIndex);

            getSessionBean1().setSelectedProcess(selectedEntry);
            getSessionBean1().getInstanceFilter().setBpName(selectedEntry.getBusinessProcessQName());
        }
    }

    public void instanceStatusFilterMenu_processValueChange(ValueChangeEvent vce) {
        Object value = vce.getNewValue();
        if (value != null && value instanceof String) {
            String status = (String) value;
            InstanceStatus2 statusFilter = null;
            if (status.equals(InstanceStatus2.RUNNING.toString())) {
                statusFilter = InstanceStatus2.RUNNING;
            } else if (status.equals(InstanceStatus2.COMPLETED.toString())) {
                statusFilter = InstanceStatus2.COMPLETED;
            } else if (status.equals(InstanceStatus2.FAULTED.toString())) {
                statusFilter = InstanceStatus2.FAULTED;
            } else if (status.equals(InstanceStatus2.SUSPENDED.toString())) {
                statusFilter = InstanceStatus2.SUSPENDED;
            } else if (status.equals(InstanceStatus2.TERMINATED.toString())) {
                statusFilter = InstanceStatus2.TERMINATED;
            }
            getSessionBean1().getInstanceFilter().setStatus(statusFilter);
        }
    }

    public String processHomeLink_action() {
        return "processMonitor";
    }
    private ListDataModel variableListDataModel = new ListDataModel();

    /**
     * @return the variableListDataModel
     */
    public ListDataModel getVariableListDataModel() {
        return variableListDataModel;
    }

    /**
     * @param variableListDataModel the variableListDataModel to set
     */
    public void setVariableListDataModel(ListDataModel variableListDataModel) {
        this.variableListDataModel = variableListDataModel;
    }

//    public void viewData_processAction(ActionEvent e) {
//
//        String instanceId = (String) FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap().get("instanceId");
//        try {
//            getSessionBean1().getMonitorbpelvariableRowSet().setObject(1, instanceId);
//        } catch (SQLException ex) {
//            Logger.getLogger(Instances.class.getName()).log(Level.SEVERE, null, ex);
//        }
//        monitorbpelvariableDataProvider.refresh();
//        variableListDataModel.setWrappedData(generateVariableDataModel(monitorbpelvariableDataProvider));
//        this.showVariableDataPopup = true;
//    }

    /*selection listener is used when a row is selected on the data table component
     *@param e RowSelectorEvent */
    public void rowSelector2_processAction(RowSelectorEvent rse) {
        UIComponent comp = rse.getComponent().getParent();
        int selectedRowIndex = rse.getRow();

        instancesDataTableCachedRowSetWrapperDataModel.setRowIndex(selectedRowIndex);
        TreeMap rowData = (TreeMap) instancesDataTableCachedRowSetWrapperDataModel.getRowData();
        String instanceId = (String) rowData.get("INSTANCEID");
        try {
            getSessionBean1().getMonitorbpelvariableRowSet().setObject(1, instanceId);
        } catch (SQLException ex) {
            Logger.getLogger(Instances.class.getName()).log(Level.SEVERE, null, ex);
        }
        monitorbpelvariableDataProvider.refresh();
        variableListDataModel.setWrappedData(generateVariableDataModel(monitorbpelvariableDataProvider));
        this.showVariableDataPopup = true;
    }

    /*selection listener is used when a row is selected on the data table component
     *@param e RowSelectorEvent */
    public void suspendResumeTerminate_processAction(ActionEvent ae) {
        String id = ae.getComponent().getId();
        int rowCount = instancesDataTableCachedRowSetWrapperDataModel.getRowCount();
        for (int i = 0; i < rowCount; i++) {
            instancesDataTableCachedRowSetWrapperDataModel.setRowIndex(i);
            TreeMap data = (TreeMap) instancesDataTableCachedRowSetWrapperDataModel.getRowData();
            Object value = data.get("SELECTED");
            if (value != null && value instanceof Boolean) {
                boolean selected = ((Boolean) value).booleanValue();
                if (selected) {
                    String instanceId = (String) data.get("INSTANCEID");
                    if (id.equals("suspend")) {
                        boolean result = getApplicationBean1().getBPELSERuntime().suspendInstance(instanceId);
                    } else if (id.equals("resume")) {
                        boolean result = getApplicationBean1().getBPELSERuntime().suspendInstance(instanceId);
                    } else if (id.equals("terminate")) {
                        boolean result = getApplicationBean1().getBPELSERuntime().terminateInstance(instanceId);
                    }
                }
            }
        }
    }

    public void suspendResumeTerminateAll_processAction(ActionEvent ae) {
        String id = ae.getComponent().getId();
        InstanceFilter filter = getSessionBean1().getInstanceFilter();
        QName selectedBusinessProcess = filter.getBpName();
        String processName = selectedBusinessProcess.toString();
        if (id.equals("suspendAll")) {
            getApplicationBean1().getBPELSERuntime().suspendAllInstance(processName);
        } else if (id.equals("resumeAll")) {
            getApplicationBean1().getBPELSERuntime().resumeAllInstance(processName);
        } else if (id.equals("terminateAll")) {
            getApplicationBean1().getBPELSERuntime().terminateAllInstance(processName);
        }
    }

    public boolean getShowVariableDataPopup() {
        return this.showVariableDataPopup;
    }

    public void setShowVariableDataPopup() {
        this.showVariableDataPopup = false;
    }

    public void closePopup(ActionEvent event) {
        showVariableDataPopup = false;
    }

    private List generateVariableDataModel(CachedRowSetDataProvider tableDataProvider) {
        List variablesList = new ArrayList();
        InstanceVariable variable = null;
        int rowCount = tableDataProvider.getRowCount();
        if (tableDataProvider != null && rowCount != -1) {
            for (int j = 0; j < rowCount; j++) {
                String instanceId = String.valueOf(tableDataProvider.getValue("VARID"));
                variable = new InstanceVariable();
                variable.setInstanceId(instanceId);
                variable.setVariableName((String) tableDataProvider.getValue("VARNAME"));
                Object value = tableDataProvider.getValue("VARVALUE");
                SerialClob clob = (SerialClob) tableDataProvider.getValue("VARVALUE");
                variable.setVariableValue(getSerializedValue(clob));
                variablesList.add(variable);
            }
        }
        return variablesList;
    }

    public String getSerializedValue(SerialClob clob) {
        StringBuffer sb = new StringBuffer();
        try {
            Reader retVal = null;
            retVal = clob.getCharacterStream();
            BufferedReader br = new BufferedReader(retVal);
            String s = null;
            while ((s = br.readLine()) != null) {
                sb.append(s);
                sb.append("\n");
            }
        } catch (IOException ex) {
            Logger.getLogger(Instances.class.getName()).log(Level.SEVERE, null, ex);
        } catch (SerialException ex) {
            Logger.getLogger(Instances.class.getName()).log(Level.SEVERE, null, ex);
        }
        return sb.toString();

    }
}