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

package org.netbeans.modules.jbi.apisupport.project;

import com.sun.source.util.Trees;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.TypeElement;
import javax.lang.model.type.TypeKind;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.ElementFilter;
import org.netbeans.api.java.classpath.ClassPath;
import org.netbeans.api.java.platform.JavaPlatform;
import org.netbeans.api.java.platform.JavaPlatformManager;
import org.netbeans.api.java.platform.Specification;
import org.netbeans.api.java.project.JavaProjectConstants;
import org.netbeans.api.java.source.CancellableTask;
import org.netbeans.api.java.source.ClassIndex;
import org.netbeans.api.java.source.ClasspathInfo;
import org.netbeans.api.java.source.CompilationController;
import org.netbeans.api.java.source.ElementHandle;
import org.netbeans.api.java.source.ElementUtilities;
import org.netbeans.api.java.source.JavaSource;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.spi.java.classpath.support.ClassPathSupport;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.util.Exceptions;

/**
 * Miscellaneous utilities for the jbi component project module.
 * @author  chikkala
 */
public class JbiCompProjectUtil {
    private JbiCompProjectUtil() {}
    
    /**
     * Returns the property value evaluated by JbiCompProject's PropertyEvaluator.
     *
     * @param p project
     * @param value of property
     * @return evaluated value of given property or null if the property not set or
     * if the project doesn't provide AntProjectHelper
     */
    public static Object getEvaluatedProperty(Project p, String value) {
        if (value == null) {
            return null;
        }
        JbiCompProject jbiCompPrj = (JbiCompProject) p.getLookup().lookup(JbiCompProject.class);
        if (jbiCompPrj != null) {
            return jbiCompPrj.evaluator().evaluate(value);
        } else {
            return null;
        }
    }
    
    /**
     * Creates an URL of a classpath or sourcepath root
     * For the existing directory it returns the URL obtained from {@link File#toUri()}
     * For archive file it returns an URL of the root of the archive file
     * For non existing directory it fixes the ending '/'
     * @param root the file of a root
     * @param offset a path relative to the root file or null (eg. src/ for jar:file:///lib.jar!/src/)"
     * @return an URL of the root
     * @throws MalformedURLException if the URL cannot be created
     */
    public static URL getRootURL(File root, String offset) throws MalformedURLException {
        URL url = root.toURI().toURL();
        if (FileUtil.isArchiveFile(url)) {
            url = FileUtil.getArchiveRoot(url);
        } else if (!root.exists()) {
            url = new URL(url.toExternalForm() + "/"); // NOI18N
        }
        if (offset != null) {
            assert offset.endsWith("/");    //NOI18N
            url = new URL(url.toExternalForm() + offset); // NOI18N
        }
        return url;
    }
    
    
    /**
     * Returns the active platform used by the project or null if the active
     * project platform is broken.
     * @param activePlatformId the name of platform used by Ant script or null
     * for default platform.
     * @return active {@link JavaPlatform} or null if the project's platform
     * is broken
     */
    public static JavaPlatform getActivePlatform(final String activePlatformId) {
        final JavaPlatformManager pm = JavaPlatformManager.getDefault();
        if (activePlatformId == null) {
            return pm.getDefaultPlatform();
        } else {
            JavaPlatform[] installedPlatforms = pm.getPlatforms(null, new Specification("j2se",null));   //NOI18N
            for (int i=0; i<installedPlatforms.length; i++) {
                String antName = (String) installedPlatforms[i].getProperties().get("platform.ant.name");        //NOI18N
                if (antName != null && antName.equals(activePlatformId)) {
                    return installedPlatforms[i];
                }
            }
            return null;
        }
    }    

    /**
     * 
     * @param intModifiers 
     * @return 
     */
    public static boolean isConcreateImpl(int intModifiers) {
        // PUBLIC && !ABSTRACT  = concreate impl.
       return  ((intModifiers & java.lang.reflect.Modifier.PUBLIC) != 0) && 
        !((intModifiers & java.lang.reflect.Modifier.ABSTRACT) != 0) ;
    }        
    /**
     * 
     * @param p 
     * @param fqn 
     * @return 
     */
    public static List /*String*/ listImplementedClasses(Project p, String fqn) {
        Set set = new HashSet(); // for non duplicate collection.
        Sources s = ProjectUtils.getSources(p);
        SourceGroup[] sg = s.getSourceGroups (JavaProjectConstants.SOURCES_TYPE_JAVA);
        FileObject root = sg[0].getRootFolder();
        return listImplementedClasses(root, fqn);
    }
    
    /**
     * 
     * @param roots 
     * @param fqn 
     * @return 
     */
    public static List /*String*/ listImplementedClasses(FileObject[] roots, String fqn) {
        Set allImpls = new HashSet();
        for ( int i=0; i < roots.length; ++i ) {
            List list = listImplementedClasses(roots[i], fqn);
            allImpls.addAll(list);
        }
        return new ArrayList(allImpls);
    }    
    /**
     * @param ifTypeEl TypeElement which must be a interface type element
     * @param ifFqn fully qualified name of the interface 
     * @return true if this type element or its parent elements are of ifFqn.
     */
    public static boolean isInterface(CompilationController ctx, TypeElement ifTypeEl, String ifFqn) {
        
        String intfName = ElementUtilities.getBinaryName(ifTypeEl);
        System.out.println("  ### Interface Name " + intfName);
        if (ifFqn.equals(intfName)) {
            return true;
        }
        // get the interfaces for this interface.
        System.out.println("  ### Checking for multiple inheritance for Interface Name " + intfName);
        List<? extends TypeMirror> ifs = ifTypeEl.getInterfaces();
        for ( TypeMirror intf : ifs ) {
            Element ifEl = ctx.getTypes().asElement(intf);
            TypeElement ifTypeEl2 = (TypeElement)ifEl;
            String intfName2 = ElementUtilities.getBinaryName(ifTypeEl2);
            System.out.println("    ### Interface Name " + intfName2);
            if (ifFqn.equals(intfName2)) {
                return true;
            }
        }
        return false;
    }
    public static boolean isInterfaceImplemented(CompilationController ctx, List<TypeMirror> clsList, String ifFqn ) {
        ElementUtilities elUtils = ctx.getElementUtilities();
        for ( TypeMirror cls : clsList) {            
            Element clsEl = ctx.getTypes().asElement(cls);
            TypeElement clsTypeEl = elUtils.enclosingTypeElement(clsEl);
            if ( clsTypeEl == null ) {
                clsTypeEl = (TypeElement)clsEl;
            }
            String clsName = ElementUtilities.getBinaryName(clsTypeEl);
//            TypeElement typeEl = ctx.getElements().getTypeElement(clsName);
            List<? extends TypeMirror> ifs = clsTypeEl.getInterfaces();
            System.out.println("##### Checking for interface impl in " + clsName);
            for ( TypeMirror intf : ifs ) {
                Element ifEl = ctx.getTypes().asElement(intf);
                TypeElement ifTypeEl = (TypeElement)ifEl;
                if ( isInterface(ctx, ifTypeEl, ifFqn)) {
                    return true;
                }
//                String intfName = ElementUtilities.getBinaryName(ifTypeEl);
//                System.out.println("  ### Interface Name " + intfName);
//                if (ifFqn.equals(intfName)) {
//                    return true;
//                }
            }
        }
        return false;
    }
    
    public static  void findSuperClassChain(CompilationController ctx, 
            TypeMirror clsTypeEl, List<TypeMirror> typeList ) {
        if ( clsTypeEl.getKind().equals(TypeKind.NONE)) {
            return;
        } else {
            typeList.add(clsTypeEl);
            TypeElement clTypeEl = (TypeElement) ctx.getTypes().asElement(clsTypeEl);
            TypeMirror superCls = clTypeEl.getSuperclass();
            findSuperClassChain(ctx, superCls, typeList);
        }
    }
    /**
     * 
     * @param clsTypeEl 
     * @param ifFqn 
     * @return 
     */
    public static boolean isConcreateImplementationOf(CompilationController ctx, TypeElement clsTypeEl, String ifFqn) {
        System.out.println("Checking for the concrete Impl of interface " + ifFqn);
        if ( clsTypeEl.getKind().isClass() && !ifFqn.equals(clsTypeEl.getQualifiedName())) {            
            Set<Modifier> modifiers = clsTypeEl.getModifiers();
            for ( Modifier modif : modifiers) {
                System.out.print(modif.name() + ",");
            }
            System.out.println("#ClassTypeElement: " + clsTypeEl);           
            if (modifiers.contains(Modifier.PUBLIC) && !modifiers.contains(Modifier.ABSTRACT)) {
                // find if this or its super classes implements the ifFqn                
                List<TypeMirror> supers = new ArrayList<TypeMirror>();
                findSuperClassChain(ctx, clsTypeEl.asType(), supers);
                if ( isInterfaceImplemented(ctx, supers, ifFqn)) {
                    return true;
                }
            }
        }
        return false;
    }
    /**
     * 
     * @param root 
     * @param fqn 
     * @return 
     */
    public static List /*String*/ listImplementedClasses(FileObject root, final String fqn) {
        Set set = new HashSet(); // for non duplicate collection.
        final List<ElementHandle<TypeElement>> result = new LinkedList<ElementHandle<TypeElement>> ();       
        try {
            ClassPath bootPath = ClassPath.getClassPath(root, ClassPath.BOOT);
            ClassPath compilePath = ClassPath.getClassPath(root, ClassPath.COMPILE);
            ClassPath srcPath = ClassPathSupport.createClassPath(new FileObject[] {root});
            ClasspathInfo cpInfo = ClasspathInfo.create(bootPath, compilePath, srcPath);
            final Set<ElementHandle<TypeElement>> classes = cpInfo.getClassIndex().getDeclaredTypes("", ClassIndex.NameKind.PREFIX, EnumSet.of(ClassIndex.SearchScope.SOURCE));
            JavaSource js = JavaSource.create(cpInfo);
            
            js.runUserActionTask(new CancellableTask<CompilationController>() {
                public void run(CompilationController control) throws Exception {
                    for (ElementHandle<TypeElement> cls : classes) {
                        TypeElement te = cls.resolve(control);
                        if (te != null) {
                            if ( isConcreateImplementationOf(control, te, fqn)){
                                result.add(cls);
                            }
                        }
                    }
                }
                
                public void cancel() {}
            }, false);
            
        } catch (IOException ioe) {
            Exceptions.printStackTrace(ioe);            
        }

        for (ElementHandle<TypeElement> clsHandler : result) {
            set.add(clsHandler.getQualifiedName());
        }
        
        return new ArrayList(set);
    }    
    
}
