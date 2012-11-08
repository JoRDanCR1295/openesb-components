/*
 * @(#)Rules4JBIActionProvider.java        $Revision: 1.10 $ $Date: 2009/01/25 21:00:54 $
 * 
 * Copyright (c) 2008, 2009 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.netbeans.project;

import java.io.File;
import java.io.IOException;
import java.util.logging.Logger;

import org.openide.DialogDisplayer;
import org.openide.NotifyDescriptor;
import org.openide.awt.StatusDisplayer;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.loaders.DataObject;
import org.openide.util.Lookup;
import org.openide.util.Mutex;
import org.openide.util.NbBundle;
import org.openide.windows.IOProvider;
import org.openide.windows.InputOutput;
import org.openide.windows.OutputWriter;

import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.spi.project.ActionProvider;
import org.netbeans.spi.project.ui.support.DefaultProjectOperations;

import org.openesb.components.rules4jbi.shared.config.Configuration;

import org.openesb.components.rules4jbi.netbeans.project.directory.DirectoryManager;
import org.openesb.components.rules4jbi.netbeans.util.JavaCompiler;
import org.openesb.components.rules4jbi.netbeans.util.FileObjectSaver;
import org.openesb.components.rules4jbi.netbeans.util.JarArchiver;
import org.openesb.components.rules4jbi.netbeans.util.JavaCompilerNotFoundException;
import org.openesb.components.rules4jbi.netbeans.util.Validator;

import org.openesb.components.rules4jbi.shared.util.FilesystemUtils;

/**
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.10 $ $Date: 2009/01/25 21:00:54 $
 * 
 * @since 0.1
 */
public class Rules4JBIActionProvider implements ActionProvider {
    
    private static final String[] SUPPORTED_ACTIONS =
            {COMMAND_BUILD, COMMAND_CLEAN, COMMAND_REBUILD, COMMAND_DELETE, COMMAND_COPY, COMMAND_MOVE,
             COMMAND_RENAME, COMMAND_COMPILE_SINGLE};
    
    private static final Logger logger = Logger.getLogger(Rules4JBIActionProvider.class.getName());
    
    private final Rules4JBIProject project;

    public Rules4JBIActionProvider(Rules4JBIProject project) {
        this.project = project;
    }

    public String[] getSupportedActions() {
        return SUPPORTED_ACTIONS;
    }

    public void invokeAction(String command, Lookup context) throws IllegalArgumentException {
        /* This code will be invoked in the event thread */
        
        logger.fine("Invoking action " + command);
        
        if (COMMAND_COPY.equals(command)) {
            DefaultProjectOperations.performDefaultCopyOperation(project);
            return;
        }

        if (COMMAND_MOVE.equals(command)) {
            DefaultProjectOperations.performDefaultMoveOperation(project);
            return;
        }

        if (COMMAND_RENAME.equals(command)) {
            DefaultProjectOperations.performDefaultRenameOperation(project, null);
            return;
        }

        if (COMMAND_DELETE.equals(command)) {
            DefaultProjectOperations.performDefaultDeleteOperation(project);
            return;
        }
        
        if (COMMAND_COMPILE_SINGLE.equals(command)) {
            DataObject dataObject = context.lookup(DataObject.class);
            
            if (dataObject == null || !Validator.isValidJavaSourceFile(dataObject.getPrimaryFile())) {
                logger.fine("No java source file found");
                
                return;
            }
            
            final File javaSourceFile = FileUtil.toFile(dataObject.getPrimaryFile());
            
            
            //TODO: put to a background thread
            ProjectManager.mutex().writeAccess(new Mutex.Action<Void>() {

                public Void run() {
                    DirectoryManager directoryManager = project.getDirectoryManager();
                    final File sourceDirectory = FileUtil.toFile(directoryManager.getSourceDirectory());
                    final File classesDirectory = FileUtil.toFile(
                            directoryManager.getOrCreateClassesDirectory());

                    InputOutput io = getIO("compile-single");
                    OutputWriter out = io.getOut();
                    OutputWriter err = io.getErr();

                    out.printf("Compiling java source file: %s%n", javaSourceFile.getAbsolutePath());

                    boolean success = false;
                    
                    try {
                        success = JavaCompiler.getCompiler().compileSingle(
                                err, sourceDirectory, classesDirectory, javaSourceFile);

                    } catch (JavaCompilerNotFoundException e) {
                        success = false;

                        err.println("Error: Could not obtain the Java(TM) programming language compiler");

                        logger.severe("Could not obtain the Java programming language compiler");
                    }
                    
                    out.println("Compilation " + (success ? "successful" : "failed"));
                    
                    err.close();
                    out.close();
                    
                    directoryManager.refreshBuildDirectory();
                    return null;
                }
            });
            
            return;
        }
        
        if (COMMAND_BUILD.equals(command)) {
            buildProject();
            
            return;
        }
        
        if (COMMAND_CLEAN.equals(command)) {
            cleanProject();
            
            return;
        }
        
        if (COMMAND_REBUILD.equals(command)) {
            cleanProject();
            buildProject();
            
            return;
        }
    }

    private void cleanProject() {
        DirectoryManager directoryManager = project.getDirectoryManager();

        directoryManager.deleteGeneratedFiles();

//        directoryManager.refreshDirectoryStructure();
    }

    private void buildProject() {
        /*
         * The configuration file we need in deployment (build) folder will
         * not be copied from r4jproject folder, but saved directly from project's lookup.
         * The config file in r4jproject is not up-to-date and is synchronized only when
         * the project is being closed. UPDATE: we now also save the configuration file
         * to be up-to-date after every build.
         */
        
        final Configuration configuration = project.getLookup().lookup(Configuration.class);
        
        final String rulesetFileName = configuration.getRulesetFile();

        if (rulesetFileName.equals("")) {

            NotifyDescriptor descriptor = new NotifyDescriptor.Message(
                    NbBundle.getMessage(Rules4JBIActionProvider.class, "ruleset.filename.not.set.message"),
                    NotifyDescriptor.ERROR_MESSAGE);

            DialogDisplayer.getDefault().notify(descriptor);

            return;
        }

        //TODO: put to a background thread
        ProjectManager.mutex().writeAccess(new Mutex.Action<Void>() {

            public Void run() {
                DirectoryManager directoryManager = project.getDirectoryManager();
                
                final FileObject buildDirectoryFileObject = directoryManager.getOrCreateBuildDirectory();
                final FileObject distDirectoryFileObject = directoryManager.getOrCreateDistDirectory();
                
                final File sourceDirectory = FileUtil.toFile(directoryManager.getSourceDirectory());
                final File classesDirectory = FileUtil.toFile(directoryManager.getOrCreateClassesDirectory());
                final File buildDirectory = FileUtil.toFile(buildDirectoryFileObject);
                final File distDirectory = FileUtil.toFile(distDirectoryFileObject);

                
                final File[] javaSourceFiles = directoryManager.getJavaSourceFiles();

                /* This name is hardcoded in compapp, we should be able to define our own name in the future*/
                final String jarFileName = "SEDeployment.jar";

                InputOutput io = getIO("build");
                OutputWriter out = io.getOut();
                OutputWriter err = io.getErr();


                /* Compiling All Files */

                out.println("Compiling all files from the source directory");

                boolean success = false;
                
                try {
                    success = JavaCompiler.getCompiler().compileAll(
                            err, sourceDirectory, classesDirectory, javaSourceFiles);
                    
                } catch (JavaCompilerNotFoundException e) {
                    success = false;
                    
                    err.println("Error: Could not obtain the Java(TM) programming language compiler");
                    
                    logger.severe("Could not obtain the Java programming language compiler");
                }

                out.println("Compilation " + (success ? "successful" : "failed"));


                /* Copying Ruleset File */

                logger.fine("Copying ruleset file '" + rulesetFileName + "' to build directory");
                out.println("Copying ruleset file '" + rulesetFileName + "' to build directory");

                FileObject rulesetFile = directoryManager.getRulesetFile(rulesetFileName);

                if (rulesetFile != null) {
                    try {
                        copyFile(rulesetFile, buildDirectoryFileObject);

                        logger.fine("Ruleset file copied successfully");

                        out.println("Ruleset file copied successfully");

                    } catch (IOException e) {
                        logger.fine("Copying of ruleset file failed: " + e.getMessage());

                        err.println("Copying of ruleset file failed: " + e.getMessage());
                    }
                } else {
                    err.println("Could not find ruleset file " + rulesetFileName);
                }

                
                /* Copying Rules Engine Libraries */
                
                logger.fine("Copying rules engine libraries to build directory");
                out.println("Copying rules engine libraries to build directory");
                
                try {
                    final File rulesEngineDirectory =
                            FileUtil.toFile(directoryManager.getRulesEngineDirectory());
                    
                    final File deploymentRulesEngineDirectory = FileUtil.toFile(
                            directoryManager.getOrCreateDeploymentRulesEngineDirectory());
                    
                    FilesystemUtils.copyDirectory(rulesEngineDirectory, deploymentRulesEngineDirectory);
                    
                    logger.fine("Rules engine libraries copied successfully");
                    
                    out.println("Rules engine libraries copied successfully");
                    
                } catch (IOException e) {
                    logger.fine("Copying of rules engine libraries failed: " + e.getMessage());

                    err.println("Copying of rules engine libraries failed: " + e.getMessage());
                }
                
                
                /* Copying Business Objects Libraries */
                
                logger.fine("Copying business objects libraries to build directory");
                out.println("Copying business objects libraries to build directory");
                
                try {
                    final File librariesDirectory = FileUtil.toFile(directoryManager.getLibrariesDirectory());
                    
                    final File deploymentLibrariesDirectory = FileUtil.toFile(
                            directoryManager.getOrCreateDeploymentLibrariesDirectory());
                    
                    FilesystemUtils.copyDirectory(librariesDirectory, deploymentLibrariesDirectory);
                    
                    logger.fine("Business objects libraries copied successfully");
                    
                    out.println("Business objects libraries copied successfully");
                    
                } catch (IOException e) {
                    logger.fine("Copying of business objects libraries failed: " + e.getMessage());

                    err.println("Copying of business objects libraries failed: " + e.getMessage());
                }
                
                
                /* Saving Configuration */

                logger.fine("Saving current configuration into build directory");
                out.println("Saving current configuration into build directory");

                try {
                    /* Update the main config file as well */
                    FileObjectSaver.save(configuration, directoryManager.getConfigFile());
                    
                    FileObjectSaver.save(configuration, directoryManager.getOrCreateDeploymentConfigFile());

                    logger.fine("Configuration saved successfully");

                    out.println("Configuration saved successfully");

                } catch (IOException e) {
                    logger.fine("Failed to save the configuration file: " + e.getMessage());

                    err.println("Failed to save the configuration file: " + e.getMessage());
                }

                
                /* Copying Service descriptions, if they exist */
                
                logger.fine("Copying service descriptions to build directory");
                out.println("Copying service descriptions to build directory");

                final FileObject wsdlFile = directoryManager.getWSDLFileIfExists();

                final FileObject descriptorFile = directoryManager.getDescriptorFileIfExists();
                
                if (wsdlFile == null) {
                    err.println("Warning: WSDL file not found");
                    
                } else if (descriptorFile == null) {
                    err.println("Warning: Deployment descriptor file not found");
                    
                } else {
                    
                    try {
                        copyFile(wsdlFile, buildDirectoryFileObject);
                        copyFile(descriptorFile, directoryManager.getOrCreateMetaInfDirectory());

                        logger.fine("Service descriptions copied successfully");

                        out.println("Service descriptions copied successfully");

                    } catch (IOException e) {
                        logger.fine("Copying of service descriptions failed: " + e.getMessage());

                        err.println("Copying of service descriptions failed: " + e.getMessage());
                    }
                }

                
                /* Creating Jar */

                logger.fine("Creating jar archive");
                out.println("Creating jar archive");
                try {
                    final FileObject jarFileObject = buildDirectoryFileObject.getFileObject(jarFileName);
                    
                    if (jarFileObject != null) {
                        logger.finer("Deleting existing jar file from build directory");
                        
                        jarFileObject.delete();
                    }
                    
                    JarArchiver.createJarArchive(buildDirectory, distDirectory, jarFileName);

                    logger.fine("Jar archive created successfully");

                    out.println("Jar archive created successfully");

                } catch (IOException e) {
                    logger.fine("Jar file creation failed: " + e.getMessage());

                    err.println("Jar file creation failed: " + e.getMessage());
                }


                /* Copying Jar */

                logger.fine("Copying jar file to build directory");
                out.println("Copying jar file to build directory");

                FileObject jarFile = distDirectoryFileObject.getFileObject(jarFileName);

                if (jarFile != null) {
                    try {
                        copyFile(jarFile, buildDirectoryFileObject);

                        logger.fine("Jar file copied successfully");

                        out.println("Jar file copied successfully");

                    } catch (IOException e) {
                        logger.fine("Copying of jar file failed: " + e.getMessage());

                        err.println("Copying of jar file failed: " + e.getMessage());
                    }
                } else {
                    err.println("Could not find the jar file");
                }


                err.close();
                out.close();

                directoryManager.refreshBuildDirectory();
                directoryManager.refreshDistDirectory();

                return null;
            }
        });

        StatusDisplayer.getDefault().setStatusText("Finished building "
                + ProjectUtils.getInformation(project).getDisplayName());
    }
    
    private void copyFile(FileObject sourceFile, FileObject destinationDirectory) throws IOException {
        assert sourceFile != null;
        assert destinationDirectory != null;
        assert sourceFile.isValid();
        assert destinationDirectory.isValid();
        assert sourceFile.isData();
        assert destinationDirectory.isFolder();
        
        final FileObject destinationFileObject = destinationDirectory.getFileObject(sourceFile.getNameExt());
        
        if (destinationFileObject != null) {
            logger.finer("Destination file " + destinationFileObject.getNameExt()
                    + " already exists - deleting it");
            
            destinationFileObject.delete();
        }
        
        sourceFile.copy(destinationDirectory, sourceFile.getName(), sourceFile.getExt());
    }
    
    private InputOutput getIO(String tabName) {
        InputOutput io = IOProvider.getDefault().getIO(
                ProjectUtils.getInformation(project).getDisplayName() + " (" + tabName + ")", false);

        io.select();

        try {
            io.getOut().reset();

        } catch (IOException e) {
            //ignore, the tab just won't get cleared
        }

        return io;
    }

    public boolean isActionEnabled(String command, Lookup context) throws IllegalArgumentException {
//        if (COMMAND_CLEAN.equals(command)) {
//            DirectoryManager directoryManager = project.getDirectoryManager();
//            
//            return directoryManager.distDirectoryExists() || !directoryManager.isBuildDirectoryEmpty();
//        }
        
        return true;
    }
}
