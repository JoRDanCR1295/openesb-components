/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)TreeBuilderBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.manager.framework.core;

import com.sun.enterprise.config.ConfigException;
import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.Util;
import com.sun.jbi.cam.manager.framework.preferences.Preferences;
import com.sun.jbi.cam.manager.framework.preferences.PreferencesFactory;
import com.sun.jbi.cam.manager.framework.status.StatusBean;
import com.sun.jbi.cam.manager.framework.status.StatusFactory;
import com.sun.jbi.cam.model.management.JBIComponentStatus;
import com.sun.jbi.cam.services.ServiceManagerFactory;
import com.sun.jbi.cam.services.administration.providers.glassfish.ServerInformation;
import com.sun.jbi.cam.services.administration.providers.glassfish.ServerInformationImpl;
import com.sun.jbi.ui.common.JBIAdminCommands;
import com.sun.webui.jsf.component.Tree;
import com.sun.webui.jsf.component.TreeNode;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.faces.component.UIComponent;

import com.sun.jbi.cam.model.management.JBIServiceAssemblyStatus;
import com.sun.jbi.cam.model.management.JBIServiceUnitStatus;
import com.sun.jbi.cam.services.ServiceManager;
import com.sun.jbi.cam.services.administration.AdministrationService;
import com.sun.jbi.cam.common.resources.Messages;
import javax.management.InstanceNotFoundException;
import javax.management.MBeanServer;
import javax.management.MBeanServerConnection;
import javax.management.MBeanServerFactory;
import javax.management.MalformedObjectNameException;

/**
 * @todo
 *   - to support enterprise-view, the application server data model needs to be defined. The tree nodes
 *     generated from this data model
 *   -
 *
 * @author ylee
 * @author graj
 */
public class TreeBuilderBean {
    
    private TreeNode rootNode = null;
    private Logger logger = Logger.getLogger(TreeBuilderBean.class.getName());
    
    private ComponentServiceProviderResolver resolver;
    
    private static String ROOT_IMGSRC = "/manager/framework/images/root.gif";  //$NON-NLS-1$
    private static String SAROOT_IMGSRC = "/manager/framework/images/sa_root.gif";  //$NON-NLS-1$
    private static String SEROOT_IMGSRC = "/manager/framework/images/se_root.gif";  //$NON-NLS-1$
    private static String BCROOT_IMGSRC = "/manager/framework/images/bc_root.gif";  //$NON-NLS-1$
    private static String SA_IMGSRC = "/manager/framework/images/sa.gif";           //$NON-NLS-1$
    private static String SU_IMGSRC = "/manager/framework/images/su.gif";           //$NON-NLS-1$
    private static String SE_IMGSRC = "/manager/framework/images/se.gif";           //$NON-NLS-1$
    private static String BC_IMGSRC = "/manager/framework/images/bc.gif";           //$NON-NLS-1$
    private static String SA_STOPPED_IMGSRC = "/manager/framework/images/sa-stopped.gif";           //$NON-NLS-1$
    private static String SU_STOPPED_IMGSRC = "/manager/framework/images/su-stopped.gif";           //$NON-NLS-1$
    private static String SE_STOPPED_IMGSRC = "/manager/framework/images/se-stopped.gif";           //$NON-NLS-1$
    private static String BC_STOPPED_IMGSRC = "/manager/framework/images/bc-stopped.gif";           //$NON-NLS-1$
    private static String SA_SHUTDOWN_IMGSRC = "/manager/framework/images/sa-shutdown.gif";           //$NON-NLS-1$
    private static String SU_SHUTDOWN_IMGSRC = "/manager/framework/images/su-shutdown.gif";           //$NON-NLS-1$
    private static String SE_SHUTDOWN_IMGSRC = "/manager/framework/images/se-shutdown.gif";           //$NON-NLS-1$
    private static String BC_SHUTDOWN_IMGSRC = "/manager/framework/images/bc-shutdown.gif";           //$NON-NLS-1$
    
    private StatusBean statusBean = null;
    
    private Preferences prefs;
    private String refreshRate = "";
    private boolean autoRefresh = false;
    
    /** Creates a new instance of TreeBuilder */
    public TreeBuilderBean() {
        resolver = ComponentServiceProviderResolver.getInstance();
        statusBean = StatusFactory.getStatusBean();
        // get preferences
        prefs = PreferencesFactory.getPreferencesInstance();
    }
    
    
    /**
     * create a tree node
     * @param String id   - unique node id e.g. x.y.z
     * @param String name - name of node
     **/
    private TreeNode createNode(String id,String name,String imgsrc,String componentType,String ctype,String cname,String parentName,String targetName) {
        TreeNode node = new TreeNode();
        node.setId(Util.replaceInvalidChars(id));
        node.setText(name);
        
        if ( parentName==null ) {
            parentName=name;
        }
        
        node.setUrl(resolver.getProviderUrl(name,componentType,ctype,cname)
        +"?"+GenericConstants.COMPONENT_NAME+"="+name+"&"
                +GenericConstants.COMPONENT_TYPE+"="+componentType+"&"
                +GenericConstants.COMPONENT_CNAME+"="+cname+"&"
                +GenericConstants.COMPONENT_CTYPE+"="+ctype+"&"
                +GenericConstants.COMPONENT_PNAME+"="+parentName+"&"
                +GenericConstants.COMPONENT_TNAME+"="+targetName
                );
        
        node.setExpanded(true);
        node.setTarget("workspaceFrame");       //$NON-NLS-1$
        if ( imgsrc!=null ) {
            node.setImageURL(imgsrc);
        }
        return node;
    }
    
    /**
     * add a tree node
     * @params String nodeId
     */
    @SuppressWarnings("unchecked")
    public String addNode(TreeNode root,String parentId,String nodeId,String nodeName,
            String imgsrc,String componentType,String ctype,String cname,String targetName) {
        UIComponent parentNode = null;
        if ( parentId!=null ) {
            parentNode = root.findComponent(parentId);
            if ( logger.isLoggable(Level.INFO) ) {
                logger.info("parentNode: "+parentId);
            }
        } else {
            parentNode = root;
        }
        
        if (parentNode != null) {
            TreeNode newNode = createNode(nodeId,nodeName,imgsrc,componentType,ctype,cname,parentId,targetName);
            List children = parentNode.getChildren();
            children.add(newNode);
            if ( logger.isLoggable(Level.INFO) ) {
                logger.info("adding node: "+newNode.getId()+" to parent node: "+parentNode.getId());
            }
        } else {
            if ( logger.isLoggable(Level.WARNING) ) {
                logger.warning("parentNode not found with id: "+parentId);
            }
        }
        return GenericConstants.SUCCESS;
    }
    
    @SuppressWarnings("unchecked")
    public String addNode(TreeNode parent, TreeNode node) {
        List children = parent.getChildren();
        children.add(node);
        return GenericConstants.SUCCESS;
    }
    
    /**
     * delete a tree node
     * @param String nodeId   -   node ID
     **/
    public String deleteNode(TreeNode root, String nodeId) {
        UIComponent node = root.findComponent(nodeId);
        if (node != null) {
            UIComponent parent = node.getParent();
            List children = parent.getChildren();
            children.remove((Object) node);
        }
        return "";
    }
    
    public TreeNode getRootNode() {
        if (rootNode == null) {
            rootNode = new TreeNode();
            //rootNode.setUrl("#");
            rootNode.setText(Messages.getString("tree_root"));
            rootNode.setId("root");     //$NON-NLS-1$
            rootNode.setExpanded(true);
            rootNode.setImageURL(ROOT_IMGSRC);
            
            buildNodes(rootNode);
            
        }
        return rootNode;
    }
    
    
    public void setRootNode(TreeNode node) {
        rootNode = null;
    }
    
    /**
     *
     * @return MBeanServer
     */
    private MBeanServer getMBeanServer() {
        MBeanServer server = null;
        ArrayList servers = MBeanServerFactory.findMBeanServer(null);
        if(servers.size() > 0) {
            server = (MBeanServer)servers.get(0);
        }
        return server;
    }
    
    ServerInformation getServerInformation() {
        ServerInformation serverInfo = null;
        MBeanServerConnection connection = null;
        try {
            connection = this.getMBeanServer();
            serverInfo = new ServerInformationImpl(connection);
        } catch (InstanceNotFoundException ex) {
            ex.printStackTrace();
        } catch (MalformedObjectNameException ex) {
            ex.printStackTrace();
        } catch (NullPointerException ex) {
            ex.printStackTrace();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        return serverInfo;
    }
    
    public void buildNodes(TreeNode root) {
        TreeNode domainNode = null;
        TreeNode node = null;
        ServerInformation serverInfo = this.getServerInformation();

        /**** remove domain node *****
        // Add domain target if it is a DAS
        if(serverInfo.isDAS() == true) {
            domainNode = new TreeNode();
            //rootNode.setUrl("#");
            domainNode.setText(JBIAdminCommands.DOMAIN_TARGET_KEY);
            domainNode.setId(JBIAdminCommands.DOMAIN_TARGET_KEY);     //$NON-NLS-1$
            domainNode.setExpanded(false);
            domainNode.setImageURL(ROOT_IMGSRC);   
            this.addNode(root, domainNode);
            this.buildNodes(domainNode, JBIAdminCommands.DOMAIN_TARGET_KEY, "componentName","componentType");     //$NON-NLS-1$
        }
        ********************************/
        
        // Add Standalone Servers
        Set<String> standaloneServerNameSet = null;
        try {
            standaloneServerNameSet = serverInfo.getStandaloneServerNames();
        } catch (ConfigException ex) {
            ex.printStackTrace();
        }
        if((standaloneServerNameSet != null) && 
           (standaloneServerNameSet.size() > 0)){
                TreeNode standaloneServersNode = null;
                standaloneServersNode = new TreeNode();
                standaloneServersNode.setText(Messages.getString("tree_standaloneServers"));
                standaloneServersNode.setId("StandaloneServers");
                standaloneServersNode.setExpanded(true);
                standaloneServersNode.setImageURL(ROOT_IMGSRC);
                this.addNode(root, standaloneServersNode);
             for (String standaloneServerName : standaloneServerNameSet) {
                node = new TreeNode();
                node.setText(standaloneServerName);
                node.setId(standaloneServerName);
                node.setExpanded(true);
                node.setImageURL(ROOT_IMGSRC);
                this.addNode(standaloneServersNode, node);
                this.buildNodes(node, standaloneServerName, null, "componentName","componentType");     //$NON-NLS-1$
            }
        }
        
        // Add Cluster Instances
        Set<String> clusterNameSet = null;
        try {
            clusterNameSet = serverInfo.getClusterNames();
        } catch (ConfigException ex) {
            ex.printStackTrace();
        }
        if((clusterNameSet != null) && 
           (clusterNameSet.size() > 0)){
                TreeNode clustersNode = null;
                clustersNode = new TreeNode();
                clustersNode.setText(Messages.getString("tree_clusters"));
                clustersNode.setId(Messages.getString("tree_clusters"));
                clustersNode.setExpanded(true);
                clustersNode.setImageURL(ROOT_IMGSRC);
                this.addNode(root, clustersNode);
            // iterate thru each cluster and list each instance
            for (String clusterName : clusterNameSet) {
                TreeNode clusterNode = new TreeNode();
                clusterNode.setText(clusterName);
                clusterNode.setId(clusterName);
                clusterNode.setExpanded(true);
                clusterNode.setImageURL(ROOT_IMGSRC);
                this.addNode(clustersNode, clusterNode);
                // get cluster instances
                try {
                  Set<String> instancesNameSet = serverInfo.getServersInCluster(clusterName);
                  for ( String instanceName : instancesNameSet ) {
                    TreeNode instanceNode = new TreeNode();
                    instanceNode.setText(instanceName);
                    instanceNode.setId(instanceName);
                    instanceNode.setExpanded(true);
                    instanceNode.setImageURL(ROOT_IMGSRC);
                    this.addNode(clusterNode, instanceNode);
                    this.buildNodes(instanceNode, clusterName, instanceName, "componentName","componentType");     //$NON-NLS-1$
                  }
                } catch(Exception e) {
                    e.printStackTrace();
                }
                //this.buildNodes(node, clusterName, "componentName","componentType");     //$NON-NLS-1$
            }
        }
    }
    
    
    public void buildNodes(TreeNode root, String targetName, String clusterName, String componentName,String componentType) {
        
        // todo - generate tree nodes from data model
        ServiceManager mgr = ServiceManagerFactory.getServiceManager(componentName,componentType);
        AdministrationService adminService;
        if ( clusterName!=null ) {
            adminService = mgr.getAdministrationService(clusterName);
        } else {
            adminService = mgr.getAdministrationService(targetName);
        }
        
        // 1. Build Service Assemblies nodes
        TreeNode saNode = getSaNode();
        addNode(root,saNode);
        buildSaNodes(adminService,saNode,targetName);
        
        // 2. Build Service Engines nodes
        TreeNode seNode = getSeNode();
        addNode(root,seNode);
        buildSeNodes(adminService,seNode, targetName);
        
        // 3. Build Binding Components nodes
        TreeNode bcNode = getBcNode();
        addNode(root,bcNode);
        buildBcNodes(adminService,bcNode, targetName);
        
    }
    
    
    public TreeNode getSaNode() {
        TreeNode saNode = new TreeNode();
        //saNode.setUrl("#");
        saNode.setText(Messages.getString("tree_serviceAssemblies"));    //$NON-NLS-1$
        saNode.setId("saRoot");     //$NON-NLS-1$
        saNode.setExpanded(true);
        saNode.setImageURL(SAROOT_IMGSRC);
        return saNode;
    }
    
    
    public TreeNode getSeNode() {
        TreeNode seNode = new TreeNode();
        //seNode.setUrl("#");
        seNode.setText(Messages.getString("tree_serviceEngines"));       //$NON-NLS-1$
        seNode.setId("seRoot");     //$NON-NLS-1$
        seNode.setExpanded(true);
        seNode.setImageURL(SEROOT_IMGSRC);
        return seNode;
    }
    
    
    public TreeNode getBcNode() {
        TreeNode bcNode = new TreeNode();
        //bcNode.setUrl("#");
        bcNode.setText(Messages.getString("tree_bindingComponents"));    //$NON-NLS-1$
        bcNode.setId("bcRoot");     //$NON-NLS-1$
        bcNode.setExpanded(true);
        bcNode.setImageURL(BCROOT_IMGSRC);
        return bcNode;
    }
    
    
    public void buildSaNodes(AdministrationService adminService,TreeNode saNode, String targetName) {
        List<JBIServiceAssemblyStatus> saList =  adminService.getServiceAssemblyList();
        Iterator<JBIServiceAssemblyStatus> saIter = saList.iterator();
        while( saIter.hasNext() ) {
            JBIServiceAssemblyStatus saStatus = saIter.next();
            String saName = saStatus.getServiceAssemblyName();
            String saState = saStatus.getStatus();
            // add to statusboard
            statusBean.setStatus(saName,GenericConstants.SU_TYPE,targetName,saName,saState);
            String saImageSrc = getStatusImageSrc(GenericConstants.SA_TYPE,saState);
            addNode(saNode,null,saName,saName,saImageSrc,GenericConstants.SA_TYPE,GenericConstants.SA_TYPE,saName,targetName);
            
            Iterator<JBIServiceUnitStatus> suIter = saStatus.getJbiServiceUnitStatusList().iterator();
            while( suIter.hasNext() ) {
                JBIServiceUnitStatus suStatus = suIter.next();
                String suName = suStatus.getServiceUnitName();
                String cName = suStatus.getTargetName();
                // add to statusboard
                String state = suStatus.getStatus();
                statusBean.setStatus(suName,GenericConstants.SU_TYPE,targetName,saName,GenericConstants.UNKNOWN_STATE);
                String imageSrc = getStatusImageSrc(GenericConstants.SU_TYPE,state);
                addNode(saNode,
                        saName,
                        Util.replaceInvalidChars(suName),
                        Util.fixupName(suName,saName),
                        imageSrc,
                        GenericConstants.SU_TYPE,
                        adminService.getComponentType(cName),
                        cName,targetName
                        );
            }
        }
        printTree(saNode);
    }
    
    public void buildSeNodes(AdministrationService mgr, TreeNode seNode, String targetName) {
        List<JBIComponentStatus> seList =  mgr.getServiceEngineList();
        Iterator<JBIComponentStatus> seIter = seList.iterator();
        while( seIter.hasNext() ) {
            JBIComponentStatus seStatus = seIter.next();
            String seName = seStatus.getName();
            // add to statusboard
            String state = seStatus.getState();
            statusBean.setStatus(seName,GenericConstants.SE_TYPE,targetName,"",state);
            String imageSrc = getStatusImageSrc(GenericConstants.SE_TYPE,state);
            addNode(seNode,null,seName,seName,imageSrc,GenericConstants.SE_TYPE,
                    GenericConstants.HASH_SEPARATOR,
                    GenericConstants.HASH_SEPARATOR,targetName);
        }
        printTree(seNode);
    }
    
    public void buildBcNodes(AdministrationService mgr, TreeNode bcNode, String targetName) {
        List<JBIComponentStatus> bcList =  mgr.getBindingComponentList();
        Iterator<JBIComponentStatus> bcIter = bcList.iterator();
        while( bcIter.hasNext() ) {
            JBIComponentStatus bcStatus = bcIter.next();
            String bcName = bcStatus.getName();
            String state = bcStatus.getState();
            // add to statusboard
            statusBean.setStatus(bcName,GenericConstants.BC_TYPE,targetName,"",state);
            String imageSrc = getStatusImageSrc(GenericConstants.BC_TYPE,state);
            addNode(bcNode,null,bcName,bcName,imageSrc,GenericConstants.BC_TYPE,
                    GenericConstants.HASH_SEPARATOR,
                    GenericConstants.HASH_SEPARATOR,targetName);
        }
        printTree(bcNode);
    }
    
    
    private String getStatusImageSrc(String componentType, String state) {
        String imageSrc = "";
        if ( GenericConstants.SE_TYPE.equals(componentType) ) {
            if ( GenericConstants.STOPPED_STATE.equals(state) ) {
                imageSrc = SE_STOPPED_IMGSRC;
            } else if ( GenericConstants.SHUTDOWN_STATE.equals(state)) {
                imageSrc = SE_SHUTDOWN_IMGSRC;
            } else {
                imageSrc = SE_IMGSRC;
            }
        } else if ( GenericConstants.BC_TYPE.equals(componentType) ) {
            if ( GenericConstants.STOPPED_STATE.equals(state)) {
                imageSrc = BC_STOPPED_IMGSRC;
            } else if ( GenericConstants.SHUTDOWN_STATE.equals(state) ) {
                imageSrc = BC_SHUTDOWN_IMGSRC;
            } else {
                imageSrc = BC_IMGSRC;
            }
        } else if ( GenericConstants.SA_TYPE.equals(componentType) ) {
            if ( GenericConstants.STOPPED_STATE.equals(state) ) {
                imageSrc = SA_STOPPED_IMGSRC;
            } else if ( GenericConstants.SHUTDOWN_STATE.equals(state) ) {
                imageSrc = SA_SHUTDOWN_IMGSRC;            
            } else {
                imageSrc = SA_IMGSRC;
            }
        } else if ( GenericConstants.SU_TYPE.equals(componentType) ) {
            if ( GenericConstants.STOPPED_STATE.equals(state) ) {
                imageSrc = SU_STOPPED_IMGSRC;
            } else if ( GenericConstants.SHUTDOWN_STATE.equals(state)) {
                imageSrc = SU_SHUTDOWN_IMGSRC;            
            } else {
                imageSrc = SU_IMGSRC;
            }
        }
        return imageSrc;
    }
    
    public String refresh() {
        if ( logger.isLoggable(Level.INFO) ) {
            logger.info("refresh...");
        }
        // reset root and subroot nodes
        setRootNode(null);
        getRootNode();
        return GenericConstants.SUCCESS;
    }
    
    public TreeNode findTreeNode(String nodeId) {
        TreeNode node = null;
        // todo?
        return node;
    }
    
    public void printTree(TreeNode rootNode) {
        if ( logger.isLoggable(Level.INFO) ) {
            logger.info("printing tree...");
        }
        if ( rootNode!=null ) {
            printTreeNode(rootNode);
            List list = rootNode.getChildren();
            for ( Iterator iter=list.iterator(); iter.hasNext();  ) {
                TreeNode node = (TreeNode)iter.next();
                printTreeNode(node);
            }
        }
    }
    
    public void printTreeNode(TreeNode node) {
        if ( logger.isLoggable(Level.INFO) ) {
            logger.info("nodeId: "+node.getId());
        }
    }
    
    public String getRefreshRate() {
        if ( logger.isLoggable(Level.INFO) ) {
            //logger.info("refreshRate...");
        }
        if ( getAutoRefresh() ) {
            refresh();
        }
        return prefs.getRefreshInterval();
    }
    
    public void setRefreshRate(String value) {
    }
    
    public boolean getAutoRefresh() {
        if ( logger.isLoggable(Level.INFO) ) {
            //logger.info("autorefresh...");
        }        
        return prefs.getAutoRefresh();
    }
    
    public void setAutoRefresh(boolean value) {
    }
    
    
}
