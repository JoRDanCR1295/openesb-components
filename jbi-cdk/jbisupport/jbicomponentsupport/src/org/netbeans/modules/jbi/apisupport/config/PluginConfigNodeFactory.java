/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.jbi.apisupport.config;

import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ui.support.NodeFactory;
import org.netbeans.spi.project.ui.support.NodeFactorySupport;
import org.netbeans.spi.project.ui.support.NodeList;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.nodes.AbstractNode;
import org.openide.nodes.Children;
import org.openide.nodes.FilterNode;
import org.openide.nodes.Node;

/**
 *
 * @author chikkala
 */
public class PluginConfigNodeFactory implements NodeFactory {
    private final static String CONFIG_FILE = "jbicomp-prj-config.xml";
    public PluginConfigNodeFactory() {
    }

    public NodeList createNodes(Project prj) {
        FileObject prjDirFO = prj.getProjectDirectory();
        FileObject configFO = prjDirFO.getFileObject(CONFIG_FILE);
        if ( configFO == null ) {
            return NodeFactorySupport.fixedNodeList();
        }        
        DataObject dataObj = null;
        ProjectConfigDataObject configDO = null;        
        try {
            dataObj = DataObject.find(configFO);
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        if ( dataObj != null && dataObj instanceof ProjectConfigDataObject) {
            configDO = (ProjectConfigDataObject)dataObj;
        }
        if ( configDO == null ) {
            return NodeFactorySupport.fixedNodeList();
        }
        
        FilterNode nd = new FilterNode(configDO.getNodeDelegate(), Children.LEAF);
        // AbstractNode nd = new AbstractNode(Children.LEAF);
        nd.setDisplayName("JBI Config");

        return NodeFactorySupport.fixedNodeList(nd);
    }
    
    public static class ConfigFilterNode extends FilterNode {

        public ConfigFilterNode(Node original) {
            super(original, Children.LEAF);
            this.disableDelegation(DELEGATE_SET_DISPLAY_NAME|DELEGATE_GET_DISPLAY_NAME);
            this.setDisplayName("JBI Config");
        }
        
    }
}
