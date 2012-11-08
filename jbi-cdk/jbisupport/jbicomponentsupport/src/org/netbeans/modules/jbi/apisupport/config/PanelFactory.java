/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.jbi.apisupport.config;

import org.netbeans.modules.jbi.apisupport.xmlmv.ui.InnerPanelFactory;
import org.netbeans.modules.jbi.apisupport.xmlmv.ui.SectionInnerPanel;
import org.netbeans.modules.jbi.apisupport.xmlmv.ui.SectionView;
import org.netbeans.modules.jbi.apisupport.xmlmv.ui.ToolBarDesignEditor;

////import org.netbeans.modules.xml.multiview.ui.InnerPanelFactory;
////import org.netbeans.modules.xml.multiview.ui.SectionInnerPanel;
////import org.netbeans.modules.xml.multiview.ui.SectionView;
////import org.netbeans.modules.xml.multiview.ui.ToolBarDesignEditor;

/**
 *
 * @author chikkala
 */
public class PanelFactory  implements InnerPanelFactory {
   
    private ProjectConfigDataObject dObj;
    private ToolBarDesignEditor editor;
    
    PanelFactory(ToolBarDesignEditor editor, ProjectConfigDataObject dObj) {
        this.dObj=dObj;
        this.editor=editor;
    }
    
    public SectionInnerPanel createInnerPanel(Object key) {
       return new ProjectConfigPanel((SectionView)editor.getContentView());
    }

}