/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.jbi.apisupport.config;

import org.openide.nodes.Node;

import org.netbeans.modules.jbi.apisupport.xmlmv.ToolBarMultiViewElement;
import org.netbeans.modules.jbi.apisupport.xmlmv.ui.SectionView;
import org.netbeans.modules.jbi.apisupport.xmlmv.ui.ToolBarDesignEditor;

////import org.netbeans.modules.xml.multiview.ToolBarMultiViewElement;
////import org.netbeans.modules.xml.multiview.ui.SectionView;
////import org.netbeans.modules.xml.multiview.ui.ToolBarDesignEditor;

/**
 *
 * @author chikkala
 */
public class ProjectConfigToolBarMVElement  extends ToolBarMultiViewElement {

    private ProjectConfigDataObject dObj;
    private SectionView view;
    private ToolBarDesignEditor comp;
    private PanelFactory factory;

    public ProjectConfigToolBarMVElement(ProjectConfigDataObject dObj) {

        super(dObj);
        this.dObj = dObj;
        comp = new ToolBarDesignEditor();
        factory = new PanelFactory(comp, dObj);
        setVisualEditor(comp);

    }

    public SectionView getSectionView() {

        return view;

    }

    public void componentShowing() {

        super.componentShowing();
        view = new TocView(dObj);
        comp.setContentView(view);
        view.open();

    }

    private class TocView extends SectionView {

        TocView(ProjectConfigDataObject dObj) {
            super(factory);
            ProjectConfigModel model = dObj.getProjectConfigModel();
            Node itemNode = new ItemNode(model);
            setRoot(itemNode);
        }

    }
    
    private class ItemNode extends org.openide.nodes.AbstractNode {

        ItemNode(ProjectConfigModel model) {
            super(org.openide.nodes.Children.LEAF);
            setDisplayName(dObj.getPrimaryFile().getNameExt());
        }

    }

}