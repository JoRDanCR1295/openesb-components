/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package bpelmonitor;

import bpelmonitor.jbiruntime.BPELSERuntime;
import bpelmonitor.jbiruntime.JBIRuntime;
import bpelmonitor.jbiruntime.ServiceAssembly;
import bpelmonitor.jbiruntime.ServiceUnit;
import com.icesoft.faces.component.tree.IceUserObject;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.faces.context.FacesContext;
import javax.servlet.http.HttpSession;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.xml.namespace.QName;

/**
 *
 * @author mbhasin
 */
public class BPList {
    // tree default bpTreeModel, used as a value for the tree component

    public static final String DASHBOARD_ENTRIES = "bpList";
    private DefaultTreeModel bpTreeModel;

    public BPList() {
        // create root node with its children expanded
        DefaultMutableTreeNode rootTreeNode = new DefaultMutableTreeNode();
        IceUserObject rootObject = new IceUserObject(rootTreeNode);
        rootObject.setText("JBI Runtime");
        rootObject.setExpanded(true);
        rootTreeNode.setUserObject(rootObject);

        // bpTreeModel is accessed by by the ice:tree component
        bpTreeModel = new DefaultTreeModel(rootTreeNode);

        List<ServiceAssembly> serviceAssemblies = new JBIRuntime().getServiceAssemblies();
        addServiceAssemblyNodes(rootTreeNode, serviceAssemblies);

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
                List<String> bps = new BPELSERuntime().getBusinessProcesses(suName);

                for (Iterator<String> bpIter = bps.iterator(); bpIter.hasNext();) {
                    String bpQNameAsString = bpIter.next();
                    QName bpQName = QName.valueOf(bpQNameAsString);

                    dashBoardEntry = new DashboardEntry();
                    dashBoardEntry.setApplicaitonName(saName);
                    dashBoardEntry.setBusinessProcessName(bpQName.toString());
                    dashboardEntries.add(dashBoardEntry);

                    addNode(suNode, bpQName.getLocalPart() + ".bpel", true);
                }
            }
        }

        FacesContext facesContext = FacesContext.getCurrentInstance();
        HttpSession session = (HttpSession) facesContext.getExternalContext().getSession(true);
        session.setAttribute(DASHBOARD_ENTRIES, dashboardEntries);
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

    /**
     * Gets the tree's default bpTreeModel.
     *
     * @return tree bpTreeModel.
     */
    public DefaultTreeModel getBpTreeModel() {
        return bpTreeModel;
    }
}
