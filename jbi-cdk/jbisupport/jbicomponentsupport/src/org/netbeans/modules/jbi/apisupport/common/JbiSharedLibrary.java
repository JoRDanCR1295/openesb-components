/*
 * JbiSharedLibrary.java
 * 
 * Created on Sep 26, 2007, 5:46:42 PM
 * 
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.netbeans.modules.jbi.apisupport.common;

import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author chikkala
 */
public interface JbiSharedLibrary {
    public static String PROP_PREFIX = "jbi.slib.";
    public static String PROP_SUFFIX = ".classpath";
    String getName();
    void setName(String name);
    String getDescription();
    void setDescription(String desc);
    String getClassPath();
    void setClassPath(String classPath);
    String getClassPathProperty();
    
    public static class JbiSharedLibraryImpl implements JbiSharedLibrary {
        private String mName;
        private String mDesc;
        private String mClassPath;
        
        public JbiSharedLibraryImpl(String name) {
            this(name, "");
        }
        public JbiSharedLibraryImpl(String name, String classpath) {
            this.mName = name;
            this.mClassPath = classpath;
            if ( this.mClassPath == null) {
                this.mClassPath = "";
            }
        }

        public static JbiSharedLibrary createFromPropertyReference(String propRef, String classPath) {
            String prop = propRef.substring("${".length(), propRef.lastIndexOf("}"));
            prop = prop.trim();
            return createFromProperty(prop, classPath);
        }
        
        public static JbiSharedLibrary createFromProperty(String prop, String classPath) {
            JbiSharedLibrary slib = null;
            String name = prop.substring(PROP_PREFIX.length(), prop.lastIndexOf(PROP_SUFFIX));
            slib = new JbiSharedLibraryImpl(name, classPath);
            return slib;
        }
        
        public static String[] getSharedLibraryNames(List<JbiSharedLibrary> slibList) {
            List<String> names = new ArrayList<String>();
            for(JbiSharedLibrary slib : slibList) {
                names.add(slib.getName());
            }
            return names.toArray(new String[0]);
        }
        
        public static List<JbiSharedLibrary> createSharedLibraries(String[] slibNames) {
            List<JbiSharedLibrary> list = new ArrayList<JbiSharedLibrary>();
            for ( String slibName : slibNames) {
                list.add(new JbiSharedLibrary.JbiSharedLibraryImpl(slibName));
            }
            return list;
        }
        
        public String getName() {
            return this.mName;
        }
        
        public void setName(String name) {
            this.mName = name;
        }

        public String getDescription() {
            return this.mDesc;
        }
        
        public void setDescription(String desc) {
            this.mDesc = desc;
        }

        public String getClassPath() {
            return this.mClassPath;                    
        }
        
        public void setClassPath(String classPath) {
            this.mClassPath = classPath;
        }

        public String getClassPathProperty() {
            return PROP_PREFIX + getName() + PROP_SUFFIX;
        }
        
        public String getClassPathPropertyReference() {
            return "${" + PROP_PREFIX + getName() + PROP_SUFFIX + "}";
        }
        
        @Override
        public String toString() {
            return "Name: " + getName() + 
                    "\nClasspath: " + getClassPath() +
                    "\n Property: " + getClassPathProperty();
        }

    }
}
