/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.openesb.components.camelse.nb.plugin.su;

import java.io.IOException;
import org.openide.filesystems.FileObject;
import org.openide.loaders.DataObjectExistsException;
import org.openide.loaders.MultiDataObject;
import org.openide.loaders.UniFileLoader;
import org.openide.util.NbBundle;

public class SUConfigDataLoader extends UniFileLoader {

    public static final String REQUIRED_MIME = "text/x-camelse-su-config+xml";
    private static final long serialVersionUID = 1L;

    public SUConfigDataLoader() {
        super("org.openesb.components.camelse.nb.plugin.su.SUConfigDataObject");
    }

    @Override
    protected String defaultDisplayName() {
        return NbBundle.getMessage(SUConfigDataLoader.class, "LBL_SUConfig_loader_name");
    }

    @Override
    protected void initialize() {
        super.initialize();
        getExtensions().addMimeType(REQUIRED_MIME);
    }

    protected MultiDataObject createMultiObject(FileObject primaryFile) throws DataObjectExistsException, IOException {
        return new SUConfigDataObject(primaryFile, this);
    }

    @Override
    protected String actionsContext() {
        return "Loaders/" + REQUIRED_MIME + "/Actions";
    }
}
