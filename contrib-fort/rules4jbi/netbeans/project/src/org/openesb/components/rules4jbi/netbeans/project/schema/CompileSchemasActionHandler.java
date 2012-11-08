/*
 * @(#)CompileSchemasActionHandler.java        $Revision: 1.2 $ $Date: 2008/11/04 18:48:13 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.project.schema;

import java.io.File;
import java.util.logging.Logger;

import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.NbBundle;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;

/**
 * This class is responsible for the business objects generation for the project corresponding to
 * the <code>DirectoryManager</code> passed to its <code>handleSchemaCompilation()</code> method.
 *
 * @author Prem Kumar
 * @version $Revision: 1.2 $ $Date: 2008/11/04 18:48:13 $
 * 
 * @since 0.2
 */
public final class CompileSchemasActionHandler {
    
    private static final Logger logger = Logger.getLogger(CompileSchemasActionHandler.class.getName());

    private static final CompileSchemasActionHandler INSTANCE = new CompileSchemasActionHandler();
    
    private CompileSchemasActionHandler() {}

    public static CompileSchemasActionHandler getInstance() {
        return INSTANCE;
    }
    
    public void handleSchemaCompilation(final DirectoryManager directoryManager) throws Exception {
        
        final FileObject sourceDirectory = directoryManager.getSourceDirectory();
        
        logger.fine("Creating business objects in " + FileUtil.getFileDisplayName(sourceDirectory));

        XMLSchemaCompiler xmlSchemaCompiler = new XMLSchemaCompiler();
        
        // generate JAXB java classes
        xmlSchemaCompiler.setDestDir(FileUtil.toFile(sourceDirectory).getAbsolutePath());    
        File[] xsdFiles = directoryManager.getSchemaFiles();
        
        if(xsdFiles.length >0){
            for(int i=0; i<xsdFiles.length; i++){
                String xsdFileAbsPath = xsdFiles[i].getAbsolutePath();
                logger.fine("Generating business objects for:"+xsdFileAbsPath);
                xsdFileAbsPath = xsdFileAbsPath.replace("\\", "/");

                // xmlSchemaCompiler.parseSchema(xsdFileAbsPath);
                
                /*
                 * TODO: this mechamism of passing the schema to the compiler
                 * needs to be completely reviewed
                 */
                xmlSchemaCompiler.parseSchema(xsdFiles[i].toURI().toString());
                
                xmlSchemaCompiler.invokeXJC();
            }

            directoryManager.refreshDirectoryStructure();
            
            NotifyDescriptor descriptor = new NotifyDescriptor.Message(
                        NbBundle.getMessage(CompileSchemasActionHandler.class, "schemas.compilation.successful.message"),
                        NotifyDescriptor.INFORMATION_MESSAGE);

            DialogDisplayer.getDefault().notify(descriptor);
            
        } else {
            logger.warning("No .xsd files were found in the schemas directory");
            
            NotifyDescriptor descriptor = new NotifyDescriptor.Message(
                        NbBundle.getMessage(CompileSchemasActionHandler.class, "no.xml.schema.files.found.message"),
                        NotifyDescriptor.ERROR_MESSAGE);

            DialogDisplayer.getDefault().notify(descriptor);
        }
    }
}
