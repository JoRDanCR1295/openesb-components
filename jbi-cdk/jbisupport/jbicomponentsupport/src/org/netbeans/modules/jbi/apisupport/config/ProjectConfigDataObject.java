/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.jbi.apisupport.config;

import java.io.IOException;
import org.netbeans.core.spi.multiview.MultiViewElement;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectExistsException;
import org.openide.loaders.MultiDataObject;
import org.openide.nodes.CookieSet;
import org.openide.nodes.Node;
import org.openide.util.Lookup;
import org.openide.text.DataEditorSupport;

import org.netbeans.modules.jbi.apisupport.xmlmv.DesignMultiViewDesc;
import org.netbeans.modules.jbi.apisupport.xmlmv.XmlMultiViewDataObject;
////import org.netbeans.modules.xml.multiview.DesignMultiViewDesc;
////import org.netbeans.modules.xml.multiview.XmlMultiViewDataObject;


 public class ProjectConfigDataObject extends XmlMultiViewDataObject {

    private final static int TYPE_TOOLBAR = 0;
    
    private ProjectConfigModel mModel = new ProjectConfigModel();

    public ProjectConfigDataObject(FileObject pf, ProjectConfigDataLoader loader) throws DataObjectExistsException, IOException {
        super(pf, loader);
        CookieSet cookies = getCookieSet();
        cookies.add((Node.Cookie) DataEditorSupport.create(this, getPrimaryEntry(), cookies));
    }

    @Override
    protected Node createNodeDelegate() {
        return new ProjectConfigDataNode(this, getLookup());
    }

    @Override
    public Lookup getLookup() {
        return getCookieSet().getLookup();
    }

    @Override
    protected DesignMultiViewDesc[] getMultiViewDesc() {
        return new DesignMultiViewDesc[]{new DesignView(this, TYPE_TOOLBAR)};
    }

    @Override
    protected String getPrefixMark() {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public ProjectConfigModel getProjectConfigModel() {
        return this.mModel;
    }

//    private static class DesignMultiViewDesc {
//        DataObject mDObj;
//        String mTitle;
//        public DesignMultiViewDesc(DataObject dObj, String title) {
//            this.mDObj = dObj;
//            this.mTitle = title;
//        }
//        public DataObject getDataObject() {
//            return this.mDObj;
//        }
//    }
    
    private static class DesignView extends DesignMultiViewDesc {

        private int type;

        DesignView(ProjectConfigDataObject dObj, int type) {
            super(dObj, "Design");
            this.type = type;
        }

        public MultiViewElement createElement() {
            ProjectConfigDataObject dObj = (ProjectConfigDataObject) getDataObject();
            return new ProjectConfigToolBarMVElement(dObj);
        }

        public java.awt.Image getIcon() {
            return org.openide.util.Utilities.loadImage("org/netbeans/modules/jbi/apisupport/config/ProjectConfigIcon.png"); //NOI18N
        }

        public String preferredID() {
            return "Toc_multiview_design" + String.valueOf(type);
        }
    }
    
}
