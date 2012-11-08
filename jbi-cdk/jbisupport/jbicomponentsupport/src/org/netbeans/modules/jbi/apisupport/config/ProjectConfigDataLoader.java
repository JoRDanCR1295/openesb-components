/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.netbeans.modules.jbi.apisupport.config;

import java.io.IOException;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObjectExistsException;
import org.openide.loaders.MultiDataObject;
import org.openide.loaders.UniFileLoader;
import org.openide.util.NbBundle;

public class ProjectConfigDataLoader extends UniFileLoader {

    public static final String REQUIRED_MIME = "text/x-jbicomp-prj-config+xml";
    private static final long serialVersionUID = 1L;

    public ProjectConfigDataLoader() {
        super("org.netbeans.modules.jbi.apisupport.config.ProjectConfigDataObject");
    }

    @Override
    protected String defaultDisplayName() {
        return NbBundle.getMessage(ProjectConfigDataLoader.class, "LBL_ProjectConfig_loader_name");
    }

    @Override
    protected void initialize() {
        super.initialize();
        getExtensions().addMimeType(REQUIRED_MIME);
    }

    protected MultiDataObject createMultiObject(FileObject primaryFile) throws DataObjectExistsException, IOException {
        return new ProjectConfigDataObject(primaryFile, this);
    }

    @Override
    protected String actionsContext() {
        return "Loaders/" + REQUIRED_MIME + "/Actions";
    }
}
