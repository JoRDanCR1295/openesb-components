/*
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the License). You may not use this file except in
 * compliance with the License.
 *
 * You can obtain a copy of the License at http://www.netbeans.org/cddl.html
 * or http://www.netbeans.org/cddl.txt.
 * 
 * When distributing Covered Code, include this CDDL Header Notice in each file
 * and include the License file at http://www.netbeans.org/cddl.txt.
 * If applicable, add the following below the CDDL Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyrighted [year] [name of copyright owner]"
 *
 * The Original Software is NetBeans. The Initial Developer of the Original
 * Software is Sun Microsystems, Inc. Portions Copyright 1997-2006 Sun
 * Microsystems, Inc. All Rights Reserved.
 */
package org.netbeans.modules.jbi.apisupport.project.classpath;

import java.io.IOException;
import java.io.File;
import java.net.URI;
import java.util.List;
import org.netbeans.modules.jbi.apisupport.project.UpdateHelper;
import org.netbeans.modules.jbi.apisupport.project.JbiCompProjectProperties;
import org.netbeans.spi.java.project.classpath.ProjectClassPathExtender;
import org.netbeans.spi.project.support.ant.PropertyUtils;
import org.openide.ErrorManager;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Mutex;
import org.netbeans.api.project.libraries.Library;
import org.netbeans.api.project.ant.AntArtifact;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.netbeans.spi.project.support.ant.ReferenceHelper;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import org.netbeans.spi.project.support.ant.EditableProperties;
import org.openide.util.MutexException;

public class JbiCompProjectClassPathExtender implements ProjectClassPathExtender {
    
    private static final String CP_CLASS_PATH = "javac.classpath"; //NOI18N

    private Project project;
    private UpdateHelper helper;
    private ReferenceHelper refHelper;
    private PropertyEvaluator eval;
    
    
    private ClassPathSupport cs;

    public JbiCompProjectClassPathExtender (Project project, UpdateHelper helper, PropertyEvaluator eval, ReferenceHelper refHelper) {
        this.project = project;
        this.helper = helper;
        this.eval = eval;
        this.refHelper = refHelper;
        
        this.cs = new ClassPathSupport( eval, refHelper, helper.getAntProjectHelper(), 
                                        JbiCompProjectProperties.WELL_KNOWN_PATHS, 
                                        JbiCompProjectProperties.LIBRARY_PREFIX, 
                                        JbiCompProjectProperties.LIBRARY_SUFFIX, 
                                        JbiCompProjectProperties.ANT_ARTIFACT_PREFIX );        
    }

    public boolean addLibrary(final Library library) throws IOException {
        return addLibrary(CP_CLASS_PATH, library);
    }

    public boolean addLibrary(final String classPathId, final Library library) throws IOException {
        assert library != null : "Parameter cannot be null";       //NOI18N
        try {
            return ((Boolean)ProjectManager.mutex().writeAccess(
                    new Mutex.ExceptionAction () {
                        public Object run() throws IOException {
                            EditableProperties props = helper.getProperties (AntProjectHelper.PROJECT_PROPERTIES_PATH);
                            String raw = props.getProperty(classPathId);
                            List resources = cs.itemsList( raw );
                            ClassPathSupport.Item item = ClassPathSupport.Item.create( library, null );
                            if (!resources.contains(item)) {
                                resources.add (item);
                                String itemRefs[] = cs.encodeToStrings( resources.iterator() );                                
                                props = helper.getProperties (AntProjectHelper.PROJECT_PROPERTIES_PATH);    //PathParser may change the EditableProperties                                
                                props.setProperty(classPathId, itemRefs);
                                helper.putProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH, props);
                                ProjectManager.getDefault().saveProject(project);
                                return Boolean.TRUE;
                            }
                            return Boolean.FALSE;
                        }
                    }
            )).booleanValue();
        } catch (MutexException e) {
            throw (IOException) e.getException();
        }
    }

    public boolean addArchiveFile(final FileObject archiveFile) throws IOException {
        return addArchiveFile(CP_CLASS_PATH,archiveFile);
    }

    public boolean addArchiveFile(final String classPathId, final FileObject archiveFile) throws IOException {
        assert archiveFile != null : "Parameter cannot be null";       //NOI18N
        try {
            return ((Boolean)ProjectManager.mutex().writeAccess(
                    new Mutex.ExceptionAction () {
                        public Object run() throws Exception {
                            EditableProperties props = helper.getProperties (AntProjectHelper.PROJECT_PROPERTIES_PATH);
                            String raw = props.getProperty(classPathId);                            
                            List resources = cs.itemsList( raw );                                                        
                            File f = FileUtil.toFile (archiveFile);
                            if (f == null ) {
                                throw new IllegalArgumentException ("The file must exist on disk");     //NOI18N
                            }
                            ClassPathSupport.Item item = ClassPathSupport.Item.create( f, null );

                            if (!resources.contains(item)) {
                                resources.add (item);
                                String itemRefs[] = cs.encodeToStrings( resources.iterator() );
                                props = helper.getProperties (AntProjectHelper.PROJECT_PROPERTIES_PATH);  //PathParser may change the EditableProperties
                                props.setProperty(classPathId, itemRefs);
                                helper.putProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH, props);
                                ProjectManager.getDefault().saveProject(project);
                                return Boolean.TRUE;
                            }
                            return Boolean.FALSE;
                        }
                    }
            )).booleanValue();
        } catch (Exception e) {
            if (e instanceof IOException) {
                throw (IOException) e;
            }
            else {
                Exception t = new IOException ();
                throw (IOException) ErrorManager.getDefault().annotate(t,e);
            }
        }
    }

    public boolean addAntArtifact(final AntArtifact artifact, final URI artifactElement) throws IOException {
        return addAntArtifact(CP_CLASS_PATH,artifact, artifactElement);
    }

    public boolean addAntArtifact(final String classPathId, final AntArtifact artifact, final URI artifactElement) throws IOException {
        assert artifact != null : "Parameter cannot be null";       //NOI18N
        try {
            return ((Boolean)ProjectManager.mutex().writeAccess(
                    new Mutex.ExceptionAction () {
                        public Object run() throws Exception {
                            EditableProperties props = helper.getProperties (AntProjectHelper.PROJECT_PROPERTIES_PATH);
                            String raw = props.getProperty (classPathId);
                            List resources = cs.itemsList( raw );
                            ClassPathSupport.Item item = ClassPathSupport.Item.create( artifact, artifactElement, null );                            
                            if (!resources.contains(item)) {
                                resources.add (item);
                                String itemRefs[] = cs.encodeToStrings( resources.iterator() );                                
                                props = helper.getProperties (AntProjectHelper.PROJECT_PROPERTIES_PATH);    //Reread the properties, PathParser changes them
                                props.setProperty (classPathId, itemRefs);
                                helper.putProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH, props);
                                ProjectManager.getDefault().saveProject(project);
                                return Boolean.TRUE;
                            }
                            return Boolean.FALSE;
                        }
                    }
            )).booleanValue();
        } catch (Exception e) {
            if (e instanceof IOException) {
                throw (IOException) e;
            }
            else {
                Exception t = new IOException ();
                throw (IOException) ErrorManager.getDefault().annotate(t,e);
            }
        }
    }

}
