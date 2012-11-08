/*
 * @(#)DirectoryManager.java        $Revision: 1.9 $ $Date: 2008/12/17 23:21:34 $
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

package org.openesb.components.rules4jbi.netbeans.project.directory;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.logging.Logger;

import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Mutex;
import org.openide.util.MutexException;

import org.netbeans.api.project.ProjectManager;

import net.jcip.annotations.GuardedBy;

import org.openesb.components.rules4jbi.netbeans.util.Validator;

import static org.openesb.components.rules4jbi.shared.GlobalConstants.CLASSES_DIR;
import static org.openesb.components.rules4jbi.shared.GlobalConstants.ENGINE_DIR;
import static org.openesb.components.rules4jbi.shared.GlobalConstants.LIBRARIES_DIR;
import static org.openesb.components.rules4jbi.shared.GlobalConstants.CONFIG_FILE_NAME;
import static org.openesb.components.rules4jbi.shared.GlobalConstants.JBI_FILE_NAME;
import static org.openesb.components.rules4jbi.shared.GlobalConstants.META_INF_DIR;

/**
 * This class is responsible for managing the directory structure (directories and files)
 * of the whole project.
 * <p>
 * A sample project's directory structure looks like this:
 * 
 * <pre>
 *   +projectDirectory
 *     +build
 *       +META-INF
 *         -jbi.xml
 *       +classes
 *         +org
 *           +example
 *             -Foo.class
 *       +lib
 *         -foobar.jar
 *       +engine
 *         -rules-engine.jar
 *       -rules.wsdl
 *       -rules-config.xml
 *       -ruleset.xml
 *     +desc
 *       -rules.wsdl
 *       -jbi.xml
 *     +dist
 *       -artifact.jar
 *     +r4jproject
 *       -rule-config.xml
 *     +nbproject
 *       -project.xml
 *     +rules
 *       -ruleset.xml
 *     +schemas
 *       -bar.xsd
 *     +engine
 *       -rules-engine.jar
 *     +lib
 *       -foobar.jar
 *     +src
 *       +org
 *         +example
 *           -Foo.java
 *     -build.xml
 * </pre>
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.9 $ $Date: 2008/12/17 23:21:34 $
 * 
 * @since 0.1
 */
public final class DirectoryManager {
    
    public static final String METADATA_DIR = "r4jproject";
    
    private static final String FAKE_METADATA_DIR = "nbproject";

    private static final String SRC_DIR = "src";
    
    private static final String RULES_DIR = "rules";
    
    private static final String SCHEMAS_DIR = "schemas";
    
    private static final String DESCRIPTIONS_DIR = "desc";
    
    private static final String DIST_DIR = "dist";
    
    private static final String BUILD_DIR = "build";
    
    private static final String FAKE_BUILD_SCRIPT_FILE_NAME = "build.xml";
    
    private static final String FAKE_CONFIG_FILE_NAME = "project.xml";
    
    /** System independent name separator character used internally by FileObject */
    private static final String FILEOBJECT_SEPARATOR = "/";
    
    private static final String CONFIG_FILE_PATH = METADATA_DIR + FILEOBJECT_SEPARATOR + CONFIG_FILE_NAME;
    
    private static final String FAKE_CONFIG_FILE_PATH =
            FAKE_METADATA_DIR + FILEOBJECT_SEPARATOR + FAKE_CONFIG_FILE_NAME;
    
    private static final String DEPLOYMENT_CONFIG_FILE_PATH =
            BUILD_DIR + FILEOBJECT_SEPARATOR + CONFIG_FILE_NAME;
    
    private static final String DEPLOYMENT_LIBRARIES_DIR_PATH =
            BUILD_DIR + FILEOBJECT_SEPARATOR + LIBRARIES_DIR;

    private static final String DEPLOYMENT_ENGINE_DIR_PATH =
            BUILD_DIR + FILEOBJECT_SEPARATOR + ENGINE_DIR;
    
    private static final String DESCRIPTOR_FILE_PATH =
            DESCRIPTIONS_DIR + FILEOBJECT_SEPARATOR + JBI_FILE_NAME;
    
    private static final String META_INF_DIR_PATH =
            BUILD_DIR + FILEOBJECT_SEPARATOR + META_INF_DIR;
    
    private static final String CLASSES_DIR_PATH = BUILD_DIR + FILEOBJECT_SEPARATOR + CLASSES_DIR;
    
    private static final Logger logger = Logger.getLogger(DirectoryManager.class.getName());
    
    private final FileObject projectDirectory;
    
    @GuardedBy("this")
    private String wsdlFileName = "";

    public DirectoryManager(final FileObject projectDirectory) {
        if (projectDirectory == null) {
            throw new NullPointerException("Project directory must not be null");
        }
        
        if (!projectDirectory.isValid() || projectDirectory.isVirtual()) {
            throw new IllegalArgumentException("Parameter projectDirectory is not valid");
        }
        
        if (!projectDirectory.isFolder()) {
            throw new IllegalArgumentException("Parameter projectDirectory must be a folder");
        }
        
        if (!projectDirectory.canRead() || !projectDirectory.canWrite()) {
            throw new IllegalArgumentException("User doesn't have access rights to the project directory");
        }
        
        logger.fine("Creating new directory manager for project directory "
                + FileUtil.getFileDisplayName(projectDirectory));
        
        this.projectDirectory = projectDirectory;
    }
    
    public synchronized void setWSDLFileName(String wsdlFileName) {
        if (wsdlFileName == null) {
            throw new NullPointerException("WSDL file name must not be null");
        }
        
        logger.fine("Setting WSDL file name to " + wsdlFileName);
        
        this.wsdlFileName = wsdlFileName;
    }

    public synchronized String getWSDLFileName() {
        return wsdlFileName;
    }
    
    public FileObject getProjectDirectory() {
        return projectDirectory;
    }
    
    public void createDirectoryStructure() throws IOException {
        try {
            ProjectManager.mutex().writeAccess(new Mutex.ExceptionAction<Void>() {
                
                public Void run() throws IOException {
                    logger.fine("Creating project's directory structure");

                    if (projectDirectory.getChildren().length > 0) {
                        throw new IllegalStateException("Project directory is not empty");
                    }

                    logger.finer("Creating source directory");
                    projectDirectory.createFolder(SRC_DIR);

                    logger.finer("Creating rules directory");
                    projectDirectory.createFolder(RULES_DIR);
                    
                    logger.finer("Creating schemas directory");
                    projectDirectory.createFolder(SCHEMAS_DIR);
                    
                    logger.finer("Creating descriptions directory");
                    projectDirectory.createFolder(DESCRIPTIONS_DIR);

                    logger.finer("Creating libraries directory");
                    projectDirectory.createFolder(LIBRARIES_DIR);
                    
                    logger.finer("Creating rules engine directory");
                    projectDirectory.createFolder(ENGINE_DIR);
                    
                    logger.finer("Creating metadata directory");
                    final FileObject metadataDirectory = projectDirectory.createFolder(METADATA_DIR);
                    
                    logger.finer("Creating configuration file");
                    metadataDirectory.createData(CONFIG_FILE_NAME);
                    
                    logger.finer("Creating fake metadata directory");
                    final FileObject fakeMetadataDirectory = projectDirectory.createFolder(FAKE_METADATA_DIR);
                    
                    logger.finer("Creating fake project configuration file");
                    fakeMetadataDirectory.createData(FAKE_CONFIG_FILE_NAME);
                    
                    logger.finer("Creating fake build script");
                    projectDirectory.createData(FAKE_BUILD_SCRIPT_FILE_NAME);
                    
                    logger.fine("Project's directory structure created successfully");
                    return null;
                }
            });

        } catch (MutexException e) {
            throw (IOException) e.getException();
        }
    }

    public boolean hasValidDirectoryStructure() {
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<Boolean>() {

            public Boolean run() {
                try {
                    getSourceDirectory();
                    getRulesDirectory();
                    getSchemasDirectory();
                    getDescriptionsDirectory();
                    getLibrariesDirectory();
                    getRulesEngineDirectory();
                    getConfigFile();
                    
                    return true;
                    
                } catch (InvalidDirectoryStructureException e) {
                    return false;
                }
            }
        });
    }
    
    public FileObject getDescriptionsDirectory() throws InvalidDirectoryStructureException {

        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {

                final FileObject result = projectDirectory.getFileObject(DESCRIPTIONS_DIR);

                if (result == null) {
                    throw new InvalidDirectoryStructureException("Descriptions directory not found");
                }

                return result;
            }
        });
    }
    
    public FileObject getRulesEngineDirectory() throws InvalidDirectoryStructureException {

        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {

                final FileObject result = projectDirectory.getFileObject(ENGINE_DIR);

                if (result == null) {
                    throw new InvalidDirectoryStructureException("Rules engine directory not found");
                }

                return result;
            }
        });
    }
    
    public FileObject getSchemasDirectory() throws InvalidDirectoryStructureException {
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                final FileObject result = projectDirectory.getFileObject(SCHEMAS_DIR);
                
                if (result == null) {
                    throw new InvalidDirectoryStructureException("Schemas directory not found");
                }
                
                return result;
            }
        });
    }
    
    public FileObject getSourceDirectory() throws InvalidDirectoryStructureException {
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                final FileObject result = projectDirectory.getFileObject(SRC_DIR);
                
                if (result == null) {
                    throw new InvalidDirectoryStructureException("Source directory not found");
                }
                
                return result;
            }
        });
    }
    
    /**
     * Returns <code>FileObject</code> representing java source file with the given fully qualified
     * class name underneath the source directory, if it exists; <code>null</code> otherwise.
     * 
     * @param fullyQualifiedClassName FQCN of the java source file
     * @return <code>FileObject</code> representing java source file with the given fully qualified
     * class name underneath the source directory, if it exists; <code>null</code> otherwise
     * @throws InvalidDirectoryStructureException if the source directory doesn't exist
     */
    public FileObject getJavaSourceFile(final String fullyQualifiedClassName)
            throws InvalidDirectoryStructureException
    {
        logger.fine("Retrieving java source file: " + fullyQualifiedClassName);
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                String javaSourceFilePath = fullyQualifiedClassName.replace(".", FILEOBJECT_SEPARATOR);
                
                javaSourceFilePath = javaSourceFilePath + ".java";
                
                logger.fine("Looking for file: " + javaSourceFilePath);
                
                final FileObject sourceDirectory = getSourceDirectory();
                
                final FileObject result = sourceDirectory.getFileObject(javaSourceFilePath);

                if (result != null) {
                    logger.fine("Found java source file: " + FileUtil.getFileDisplayName(result));
                }
                
                return result;
            }
        });
    }
    
    public File[] getJavaSourceFiles() throws InvalidDirectoryStructureException {
        logger.fine("Retrieving all java source files");
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<File[]>() {

            public File[] run() {
                
                final FileObject sourceDirectory = getSourceDirectory();
                
                Enumeration<? extends FileObject> data = sourceDirectory.getData(true);

                List<File> javaSourceFiles = new ArrayList<File>();

                while (data.hasMoreElements()) {
                    FileObject fileObject = data.nextElement();

                    if (Validator.isValidJavaSourceFile(fileObject)) {
                        File javaSourceFile = FileUtil.toFile(fileObject);

                        logger.finer("Found java source file: " + javaSourceFile.getAbsolutePath());

                        javaSourceFiles.add(javaSourceFile);
                    }
                }

                File[] result = javaSourceFiles.toArray(new File[javaSourceFiles.size()]);
                
                return result;
            }
        });
    }
    
    public File[] getSchemaFiles() throws InvalidDirectoryStructureException {
        logger.fine("Retrieving all xsd files from schemas directory");
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<File[]>() {

            public File[] run() {
                
                final FileObject schemasDirectory = getSchemasDirectory();
                
                Enumeration<? extends FileObject> data = schemasDirectory.getData(true);
                
                List<File> schemaFiles = new ArrayList<File>();

                while (data.hasMoreElements()) {
                    FileObject fileObject = data.nextElement();
                    
                    if (fileObject.getExt().equals("xsd")) {
                        File schemaFile = FileUtil.toFile(fileObject);
                        logger.fine("Found schema file: " + schemaFile.getName());
                        schemaFiles.add(schemaFile);
                    }
                }

                File[] result = schemaFiles.toArray(new File[schemaFiles.size()]);
                
                return result;
            }
        });
    }
    
    public FileObject getLibrariesDirectory() throws InvalidDirectoryStructureException {
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                final FileObject result = projectDirectory.getFileObject(LIBRARIES_DIR);
                
                if (result == null) {
                    throw new InvalidDirectoryStructureException("Libraries directory not found");
                }
                
                return result;
            }
        });
    }
    
    public FileObject getRulesDirectory() throws InvalidDirectoryStructureException {
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                final FileObject result = projectDirectory.getFileObject(RULES_DIR);
                
                if (result == null) {
                    throw new InvalidDirectoryStructureException("Rules directory not found");
                }
                
                return result;
            }
        });
    }
    
    public FileObject getRulesetFile(final String rulesetFileName)
            throws InvalidDirectoryStructureException
    {
        logger.fine("Retrieving ruleset file " + rulesetFileName);
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                final FileObject rulesDirectory = getRulesDirectory();
                
                final FileObject result = rulesDirectory.getFileObject(rulesetFileName);

                if (result != null) {
                    logger.finer("Found ruleset file " + FileUtil.getFileDisplayName(result));
                    
                } else {
                    logger.fine("Ruleset file not found");
                }
                
                return result;
            }
        });
    }

    public FileObject getFakeMetadataDirectory() throws InvalidDirectoryStructureException {

        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {

                final FileObject result = projectDirectory.getFileObject(FAKE_METADATA_DIR);

                if (result == null) {
                    throw new InvalidDirectoryStructureException("Fake metadata directory not found");
                }

                return result;
            }
        });
    }
    
    public FileObject getMetadataDirectory() throws InvalidDirectoryStructureException {
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                final FileObject result = projectDirectory.getFileObject(METADATA_DIR);
                
                if (result == null) {
                    throw new InvalidDirectoryStructureException("Metadata directory not found");
                }
                
                return result;
            }
        });
    }
    
    public FileObject getConfigFile() throws InvalidDirectoryStructureException {
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                FileObject result = projectDirectory.getFileObject(CONFIG_FILE_PATH);
                
                if (result == null) {
                    throw new InvalidDirectoryStructureException("Configuration file not found");
                }
                
                return result;
            }
        });
    }
    
    public FileObject getFakeConfigFile() throws InvalidDirectoryStructureException {

        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {

                FileObject result = projectDirectory.getFileObject(FAKE_CONFIG_FILE_PATH);

                if (result == null) {
                    throw new InvalidDirectoryStructureException("Fake project configuration file not found");
                }

                return result;
            }
        });
    }

    public FileObject getFakeBuildScriptFile() throws InvalidDirectoryStructureException {

        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {

                FileObject result = projectDirectory.getFileObject(FAKE_BUILD_SCRIPT_FILE_NAME);

                if (result == null) {
                    throw new InvalidDirectoryStructureException("Fake build script file not found");
                }

                return result;
            }
        });
    }
    
    public boolean distDirectoryExists() {
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<Boolean>() {

            public Boolean run() {
                return projectDirectory.getFileObject(DIST_DIR) != null;
            }
        });
    }
    
    public boolean buildDirectoryExists() {
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<Boolean>() {

            public Boolean run() {
                return projectDirectory.getFileObject(BUILD_DIR) != null;
            }
        });
    }

    public boolean isBuildDirectoryEmpty() {
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<Boolean>() {

            public Boolean run() {
                final FileObject buildDirectory = projectDirectory.getFileObject(BUILD_DIR);

                if (buildDirectory != null) {
                    FileObject[] buildDirectoryContent = buildDirectory.getChildren();
                    
                    return buildDirectoryContent.length == 0;
                    
                } else {
                    return true;
                }
            }
        });
    }
    
    public boolean classesDirectoryExists() {
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<Boolean>() {

            public Boolean run() {
                return projectDirectory.getFileObject(CLASSES_DIR_PATH) != null;
            }
        });
    }
    
    public boolean wsdlFileExists() {
        final String currentWSDLFileName = getWSDLFileName();
        
        if (currentWSDLFileName.trim().equals("") || !currentWSDLFileName.endsWith(".wsdl")) {
            return false;
        }
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<Boolean>() {

            public Boolean run() {
                
                return projectDirectory.getFileObject(
                        DESCRIPTIONS_DIR + FILEOBJECT_SEPARATOR + currentWSDLFileName) != null;
            }
        });
    }

    public boolean descriptorFileExists() {

        return ProjectManager.mutex().readAccess(new Mutex.Action<Boolean>() {

            public Boolean run() {

                return projectDirectory.getFileObject(DESCRIPTOR_FILE_PATH) != null;
            }
        });
    }
    
    public FileObject getOrCreateDistDirectory() throws RuntimeIOException {
        logger.finer("Retrieving dist directory");
        
        FileObject distDirectory = ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {

                return projectDirectory.getFileObject(DIST_DIR);
            }
        });
        
        if (distDirectory != null) {
            logger.finer("Found existing dist directory");
            return distDirectory;
            
        } else {
            return ProjectManager.mutex().writeAccess(new Mutex.Action<FileObject>() {

                public FileObject run() {
                    FileObject result = projectDirectory.getFileObject(DIST_DIR);

                    if (result == null) {
                        logger.finer("Creating dist directory");

                        try {
                            result = projectDirectory.createFolder(DIST_DIR);
                            logger.finest("Dist directory created successfully");

                        } catch (IOException e) {
                            throw new RuntimeIOException("Failed to create dist directory", e);
                        }
                    }

                    return result;
                }
            });
        }
    }
    
    public FileObject getOrCreateBuildDirectory() throws RuntimeIOException {
        logger.finer("Retrieving build directory");
        
        FileObject buildDirectory = ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {

                return projectDirectory.getFileObject(BUILD_DIR);
            }
        });
        
        if (buildDirectory != null) {
            logger.finer("Found existing build directory");
            return buildDirectory;
            
        } else {
            return ProjectManager.mutex().writeAccess(new Mutex.Action<FileObject>() {

                public FileObject run() {
                    FileObject result = projectDirectory.getFileObject(BUILD_DIR);

                    if (result == null) {
                        logger.finer("Creating build directory");

                        try {
                            result = projectDirectory.createFolder(BUILD_DIR);
                            logger.finest("Build directory created successfully");

                        } catch (IOException e) {
                            throw new RuntimeIOException("Failed to create build directory", e);
                        }
                    }

                    return result;
                }
            });
        }
    }
    
    public FileObject getOrCreateClassesDirectory() throws RuntimeIOException {
        logger.fine("Retrieving classes directory");
        
        FileObject classesDirectory = ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                return projectDirectory.getFileObject(CLASSES_DIR_PATH);
            }
        });
        
        if (classesDirectory != null) {
            logger.finer("Found existing classes directory");
            
            return classesDirectory;
        }
        
        return ProjectManager.mutex().writeAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                final FileObject buildDirectory = getOrCreateBuildDirectory();

                FileObject result = buildDirectory.getFileObject(CLASSES_DIR);

                if (result == null) {
                    logger.finer("Creating classes directory");

                    try {
                        result = buildDirectory.createFolder(CLASSES_DIR);
                        logger.finest("Classes directory created successfully");
                        
                    } catch (IOException e) {
                        throw new RuntimeIOException("Failed to create classes directory", e);
                    }
                }

                return result;
            }
        });
    }
    
    public FileObject getOrCreateDeploymentLibrariesDirectory() throws RuntimeIOException {
        logger.fine("Retrieving deployment libraries directory");
        
        FileObject librariesDirectory = ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                return projectDirectory.getFileObject(DEPLOYMENT_LIBRARIES_DIR_PATH);
            }
        });
        
        if (librariesDirectory != null) {
            logger.finer("Found existing deployment libraries directory");
            
            return librariesDirectory;
        }
        
        return ProjectManager.mutex().writeAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                final FileObject buildDirectory = getOrCreateBuildDirectory();

                FileObject result = buildDirectory.getFileObject(LIBRARIES_DIR);

                if (result == null) {
                    logger.finer("Creating deployment libraries directory");

                    try {
                        result = buildDirectory.createFolder(LIBRARIES_DIR);
                        logger.finest("Deployment libraries directory created successfully");
                        
                    } catch (IOException e) {
                        throw new RuntimeIOException("Failed to create deployment libraries directory", e);
                    }
                }

                return result;
            }
        });
    }
    
    public FileObject getOrCreateDeploymentRulesEngineDirectory() throws RuntimeIOException {
        logger.fine("Retrieving deployment rules engine directory");
        
        FileObject engineDirectory = ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                return projectDirectory.getFileObject(DEPLOYMENT_ENGINE_DIR_PATH);
            }
        });
        
        if (engineDirectory != null) {
            logger.finer("Found existing deployment rules engine directory");
            
            return engineDirectory;
        }
        
        return ProjectManager.mutex().writeAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                final FileObject buildDirectory = getOrCreateBuildDirectory();

                FileObject result = buildDirectory.getFileObject(ENGINE_DIR);

                if (result == null) {
                    logger.finer("Creating deployment rules engine directory");

                    try {
                        result = buildDirectory.createFolder(ENGINE_DIR);
                        logger.finest("Deployment rules engine directory created successfully");
                        
                    } catch (IOException e) {
                        throw new RuntimeIOException("Failed to create deployment rules engine directory", e);
                    }
                }

                return result;
            }
        });
    }
    
    public FileObject getDescriptorFileIfExists() {
        logger.fine("Retrieving descriptor file (if exists)");
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                return projectDirectory.getFileObject(DESCRIPTOR_FILE_PATH);
            }
        });
    }
    
    public FileObject getOrCreateMetaInfDirectory() throws RuntimeIOException {
        logger.fine("Retrieving META-INF directory");
        
        FileObject metaInfDirectory = ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                return projectDirectory.getFileObject(META_INF_DIR_PATH);
            }
        });
        
        if (metaInfDirectory != null) {
            logger.finer("Found existing META-INF directory");
            
            return metaInfDirectory;
        }
        
        return ProjectManager.mutex().writeAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                try {
                    final FileObject buildDirectory = getOrCreateBuildDirectory();

                    FileObject result = buildDirectory.getFileObject(META_INF_DIR);

                    if (result == null) {
                        logger.finer("Creating META-INF directory");
                        result = buildDirectory.createFolder(META_INF_DIR);
                        logger.finest("META-INF directory created successfully");
                    }

                    return result;
                    
                } catch (IOException e) {
                    throw new RuntimeIOException("Failed to create META-INF directory", e);
                }
            }
        });
    }
    
    public FileObject getOrCreateDescriptorFile() throws InvalidDirectoryStructureException, RuntimeIOException
    {
        logger.fine("Retrieving descriptor file");
        
        FileObject descriptorFile = ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                return projectDirectory.getFileObject(DESCRIPTOR_FILE_PATH);
            }
        });
        
        if (descriptorFile != null) {
            logger.finer("Found existing descriptor file");
            
            return descriptorFile;
        }
        
        return ProjectManager.mutex().writeAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                try {
                    final FileObject descriptionsDirectory = getDescriptionsDirectory();

                    logger.finer("Retrieving descriptor file");
                    FileObject result = descriptionsDirectory.getFileObject(JBI_FILE_NAME);

                    if (result == null) {
                        logger.finer("Creating descriptor file");
                        result = descriptionsDirectory.createData(JBI_FILE_NAME);
                        logger.finest("Descriptor file created successfully");
                    }

                    return result;
                    
                } catch (IOException e) {
                    throw new RuntimeIOException("Failed to create descriptor file", e);
                }
            }
        });
    }
    
    public FileObject getOrCreateDeploymentConfigFile() throws RuntimeIOException {
        logger.fine("Retrieving deployment configuration file");
        
        FileObject deploymentConfig = ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                return projectDirectory.getFileObject(DEPLOYMENT_CONFIG_FILE_PATH);
            }
        });
        
        if (deploymentConfig != null) {
            logger.finer("Found existing deployment configuration file");
            
            return deploymentConfig;
        }
        
        return ProjectManager.mutex().writeAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                try {
                    final FileObject buildDirectory = getOrCreateBuildDirectory();

                    FileObject result = buildDirectory.getFileObject(CONFIG_FILE_NAME);

                    if (result == null) {
                        logger.finer("Creating deployment configuration file");
                        result = buildDirectory.createData(CONFIG_FILE_NAME);
                        logger.finest("Deployment configuration file created successfully");
                    }

                    return result;
                    
                } catch (IOException e) {
                    throw new RuntimeIOException("Failed to create deployment configuration file", e);
                }
            }
        });
    }    
    
    public FileObject getWSDLFileIfExists() {
        final String currentWSDLFileName = getWSDLFileName();
        
        logger.fine("Retrieving WSDL file (if exists) " + currentWSDLFileName);
        
        if (currentWSDLFileName.trim().equals("") || !currentWSDLFileName.endsWith(".wsdl")) {
            return null;
        }
        
        return ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                return projectDirectory.getFileObject(
                        DESCRIPTIONS_DIR + FILEOBJECT_SEPARATOR + currentWSDLFileName);
            }
        });
    }
    
    public FileObject getOrCreateWSDLFile() throws RuntimeIOException, InvalidDirectoryStructureException {
        final String currentWSDLFileName = getWSDLFileName();
        
        logger.fine("Retrieving WSDL file " + currentWSDLFileName);
        
        if (currentWSDLFileName.trim().equals("") || !currentWSDLFileName.endsWith(".wsdl")) {
            throw new IllegalStateException("WSDL file name is not properly set");
        }
        
        final FileObject wsdlFile = ProjectManager.mutex().readAccess(new Mutex.Action<FileObject>() {

            public FileObject run() {
                
                return projectDirectory.getFileObject(
                        DESCRIPTIONS_DIR + FILEOBJECT_SEPARATOR + currentWSDLFileName);
            }
        });
        
        if (wsdlFile != null) {
            logger.finer("Found existing WSDL file");
            
            return wsdlFile;
        }
        
        return ProjectManager.mutex().writeAccess(new Mutex.Action<FileObject>() {
            
             public FileObject run() {
                 final FileObject descriptionsDirectory = getDescriptionsDirectory();
                 
                 FileObject result = descriptionsDirectory.getFileObject(currentWSDLFileName);
                 
                 if (result == null) {
                     logger.finer("Creating WSDL file");

                     try {
                         result = descriptionsDirectory.createData(currentWSDLFileName);
                         logger.finest("WSDL file created successfully");

                     } catch (IOException e) {
                         throw new RuntimeIOException("Failed to create WSDL file", e);
                     }
                 }
                 
                 return result;
             }
        });
    }
    
    public void deleteGeneratedFiles() throws RuntimeIOException {
        logger.fine("Deleting generated files and folders");
        
        ProjectManager.mutex().writeAccess(new Mutex.Action<Void>() {

            public Void run() {
                try {
                    logger.finer("Deleting dist directory");
                    final FileObject distDirectory = projectDirectory.getFileObject(DIST_DIR);

                    if (distDirectory != null) {
                        distDirectory.delete();
                    }

                    logger.finer("Deleting build directory");
                    final FileObject buildDirectory = projectDirectory.getFileObject(BUILD_DIR);
                    
                    if (buildDirectory != null) {
                        buildDirectory.delete();
                    }

                    return null;

                } catch (IOException e) {
                    throw new RuntimeIOException("Failed to delete generated files", e);
                }
            }
        });

        logger.fine("Generated files deleted successfully");
    }

    public void refreshDirectoryStructure() throws IOException {
        projectDirectory.getFileSystem().refresh(true);
    }

    public void refreshBuildDirectory() {
        
        ProjectManager.mutex().readAccess(new Mutex.Action<Void>() {

            public Void run() {
                if (buildDirectoryExists()) {
                    final FileObject buildDirectory = getOrCreateBuildDirectory();
                    
                    Enumeration<? extends FileObject> folders = buildDirectory.getFolders(true);

                    while (folders.hasMoreElements()) {
                        folders.nextElement().refresh(true);
                    }
                } else {
                    projectDirectory.refresh(true);
                }
                
                return null;
            }
        });
    }

    public void refreshDistDirectory() {
        
        ProjectManager.mutex().readAccess(new Mutex.Action<Void>() {

            public Void run() {
                if (distDirectoryExists()) {
                    
                    final FileObject distDirectory = getOrCreateDistDirectory();
                    
                    distDirectory.refresh(true);
                    
                } else {
                    projectDirectory.refresh(true);
                }
                
                return null;
            }
        });
    }
}
