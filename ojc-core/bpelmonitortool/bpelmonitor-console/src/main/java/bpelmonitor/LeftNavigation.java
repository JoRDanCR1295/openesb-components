/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package bpelmonitor;

import bpelmonitor.model.DashboardEntry;
import com.icesoft.faces.component.jsfcl.data.DefaultTree;
import com.sun.rave.web.ui.appbase.AbstractFragmentBean;
import javax.faces.FacesException;
import bpelmonitor.jbiruntime.BPELSERuntime;
import bpelmonitor.jbiruntime.JBIRuntime;
import bpelmonitor.model.ServiceAssembly;
import bpelmonitor.model.ServiceUnit;
import com.icesoft.faces.component.tree.IceUserObject;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.xml.namespace.QName;

/**
 * <p>Fragment bean that corresponds to a similarly named JSP page
 * fragment.  This class contains component definitions (and initialization
 * code) for all components that you have defined on this fragment, as well as
 * lifecycle methods and event handlers where you may add behavior
 * to respond to incoming events.</p>
 *
 * @version LeftNavigation.java
 * @version Created on May 5, 2009, 5:03:09 PM
 * @author mbhasin
 */
public class LeftNavigation extends AbstractFragmentBean {
    // <editor-fold defaultstate="collapsed" desc="Managed Component Definition">

    /**
     * <p>Automatically managed component initialization. <strong>WARNING:</strong>
     * This method is automatically generated, so any user-specified code inserted
     * here is subject to being replaced.</p>
     */
    private void _init() throws Exception {
    }
    private DefaultTree tree2Model = new DefaultTree();

    public DefaultTree getTree2Model() {
        return tree2Model;
    }

    public void setTree2Model(DefaultTree dt) {
        this.tree2Model = dt;
    }
    // </editor-fold>

    public LeftNavigation() {
    }

    /**
     * <p>Callback method that is called whenever a page containing
     * this page fragment is navigated to, either directly via a URL,
     * or indirectly via page navigation.  Override this method to acquire
     * resources that will be needed for event handlers and lifecycle methods.</p>
     * 
     * <p>The default implementation does nothing.</p>
     */
    @Override
    public void init() {
        // Perform initializations inherited from our superclass
        super.init();
        // Perform application initialization that must complete
        // *before* managed components are initialized
        // TODO - add your own initialiation code here


        // <editor-fold defaultstate="collapsed" desc="Visual-Web-managed Component Initialization">
        // Initialize automatically managed components
        // *Note* - this logic should NOT be modified
        try {
            _init();
        } catch (Exception e) {
            log("Page1 Initialization Failure", e);
            throw e instanceof FacesException ? (FacesException) e : new FacesException(e);
        }

        // </editor-fold>
        // Perform application initialization that must complete
        // *after* managed components are initialized
        // TODO - add your own initialization code here
        getTree2Model().setModel(getSessionBean1().getBpTreeModel());

    }

    /**
     * <p>Callback method that is called after rendering is completed for
     * this request, if <code>init()</code> was called.  Override this
     * method to release resources acquired in the <code>init()</code>
     * resources that will be needed for event handlers and lifecycle methods.</p>
     * 
     * <p>The default implementation does nothing.</p>
     */
    @Override
    public void destroy() {
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

    /**
     * <p>Return a reference to the scoped data bean.</p>
     *
     * @return reference to the scoped data bean
     */
    protected SessionBean1 getSessionBean1() {
        return (SessionBean1) getBean("SessionBean1");
    }

    public String commandLink1_action() {
        //return null means stay on the same page
        return null;
    }

    public String commandLink2_action() {
        //return null means stay on the same page
        return null;
    }
}
