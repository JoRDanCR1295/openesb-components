/*
 * ProcessMonitor.java
 *
 * Created on May 6, 2009, 12:05:23 PM
 * Copyright mbhasin
 */
package bpelmonitor;

import bpelmonitor.model.DashboardEntry;
import bpelmonitor.model.InstanceFilter;
import bpelmonitor.model.InstanceFilter.InstanceStatus2;
import com.icesoft.faces.component.DisplayEvent;
import com.icesoft.faces.component.ext.HtmlCommandLink;
import com.sun.rave.web.ui.appbase.AbstractPageBean;
import javax.faces.FacesException;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;

/**
 * <p>Page bean that corresponds to a similarly named JSP page.  This
 * class contains component definitions (and initialization code) for
 * all components that you have defined on this page, as well as
 * lifecycle methods and event handlers where you may add behavior
 * to respond to incoming events.</p>
 */
public class ProcessMonitor extends AbstractPageBean {

    public static final String LIVE_INSTANCES_HEADER = "Live Instances";
    public static final String COMPLETED_INSTANCES_HEADER = "Completed Instances";
    public static final String SUSPENDED_INSTANCES_HEADER = "Suspended Instances";
    public static final String FAULTED_INSTANCES_HEADER = "Faulted Instances";
    public static final String TERMINATED_INSTANCES_HEADER = "Terminated Instances";

    // NOTE: Dont change the string values of the following ID without changing
    // the correspoding in the JSP page - ProcessMonitor.jsp
    public static final String LIST_ALL_UI_ID = "listAll";
    public static final String MOST_RECENT_UI_ID = "mostRecent";
    public static final String OLDEST_UI_ID = "oldest";
    public static final String SEARCH_INSTANCES_UI_ID = "search";

    // NOTE: Dont change the string values of the following ID without changing
    // the correspoding in the JSP page - ProcessMonitor.jsp
    public static String MOST_RECENT_INSTANCE_DISPLAY = "Most Recent Instance";
    public static String OLDEST_INSTANCE_DISPLAY = "Oldest Instance";
    public static String ALL_AVAILABLE_INSTANCES_DISPLAY = "All Available Instances";
    public static String SEARCH_INSTANCES_DISPLAY = "Search Instances";

    // NOTE: Dont change the string value of the following without changing
    // the correspinding in the faces-config.xml
    private static final String INSTANCES_NAVIGATION = "instances";
    private static final String INSTANCES_NAVIGATION1 = "instances1";
    private static final String SEARCH_INSTANCES_NAVIATION = "searchInstances";
    public String optionsHeader = null;
    // <editor-fold defaultstate="collapsed" desc="Managed Component Definition">
    private int __placeholder;

    /**
     * <p>Automatically managed component initialization.  <strong>WARNING:</strong>
     * This method is automatically generated, so any user-specified code inserted
     * here is subject to being replaced.</p>
     */
    private void _init() throws Exception {
    }

    // </editor-fold>
    /**
     * <p>Construct a new Page bean instance.</p>
     */
    public ProcessMonitor() {
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
            log("ProcessMonitor Initialization Failure", e);
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
    protected ApplicationBean1 getApplicationBean1() {
        return (ApplicationBean1) getBean("ApplicationBean1");
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
     * @param event
     */
    public void optionsListener(DisplayEvent event) {
        String contextValue = (String) event.getContextValue();
        if (contextValue.equals("liveInstances")) {
            optionsHeader = LIVE_INSTANCES_HEADER;
        } else if (contextValue.equals("completedInstances")) {
            optionsHeader = COMPLETED_INSTANCES_HEADER;
        } else if (contextValue.equals("suspendedInstances")) {
            optionsHeader = SUSPENDED_INSTANCES_HEADER;
        } else if (contextValue.equals("faultedInstances")) {
            optionsHeader = FAULTED_INSTANCES_HEADER;
        } else if (contextValue.equals("terminatedInstances")) {
            optionsHeader = TERMINATED_INSTANCES_HEADER;
        }
    }

    public void commandLinkListener(ActionEvent e) {
        UIComponent comp = e.getComponent();
        FacesContext facesContext = FacesContext.getCurrentInstance();

        if (comp instanceof HtmlCommandLink) {
            DashboardEntry entry = getSessionBean1().getSelectedProcess();
            InstanceFilter filter = new InstanceFilter(entry.getBusinessProcessQName());

            if (optionsHeader.equals(LIVE_INSTANCES_HEADER)) {
                //filter.setStatus(InstanceStatus.RUNNING);
                filter.setStatus(InstanceStatus2.RUNNING);
            } else if (optionsHeader.equals(COMPLETED_INSTANCES_HEADER)) {
                filter.setStatus(InstanceStatus2.COMPLETED);
            } else if (optionsHeader.equals(SUSPENDED_INSTANCES_HEADER)) {
                filter.setStatus(InstanceStatus2.SUSPENDED);
            } else if (optionsHeader.equals(FAULTED_INSTANCES_HEADER)) {
                filter.setStatus(InstanceStatus2.FAULTED);
            } else if (optionsHeader.equals(TERMINATED_INSTANCES_HEADER)) {
                filter.setStatus(InstanceStatus2.TERMINATED);
            }

            HtmlCommandLink commandLink = (HtmlCommandLink) comp;

            String id = commandLink.getId();
            if (id.equals(LIST_ALL_UI_ID)) {
                filter.setSubFilter(getAllAvailableDisplay());
            } else if (id.equals(MOST_RECENT_UI_ID)) {
                filter.setSubFilter(getMostRecentInstanceDisplay());
            } else if (id.equals(OLDEST_UI_ID)) {
                filter.setSubFilter(getOldestInstanceDisplay());
            } else if (id.equals(SEARCH_INSTANCES_UI_ID)) {
            }

            getSessionBean1().setInstanceFilter(filter);
            facesContext.getApplication().getNavigationHandler().handleNavigation(facesContext, null, INSTANCES_NAVIGATION);
        }
    }

    public String getMostRecentInstanceDisplay() {
        return MOST_RECENT_INSTANCE_DISPLAY;
    }

    public String getOldestInstanceDisplay() {
        return OLDEST_INSTANCE_DISPLAY;
    }

    public String getAllAvailableDisplay() {
        return ALL_AVAILABLE_INSTANCES_DISPLAY;
    }

    public String getSearchInstancesDisplay() {
        return SEARCH_INSTANCES_DISPLAY;
    }
    
    public String getOptionsHeader() {
        return optionsHeader;
    }
}

