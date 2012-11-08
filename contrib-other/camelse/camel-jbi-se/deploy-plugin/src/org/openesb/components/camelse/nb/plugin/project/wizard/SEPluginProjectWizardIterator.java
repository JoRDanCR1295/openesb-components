/*
 * SEPluginProjectWizardIterator.java
 */
package org.openesb.components.camelse.nb.plugin.project.wizard;

import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.StringTokenizer;
import javax.swing.JComponent;
import javax.swing.event.ChangeListener;
import org.openesb.components.camelse.nb.plugin.project.SEPluginProjectGenerator;
import org.openesb.components.camelse.nb.plugin.project.SEPluginProjectProperties;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.netbeans.spi.project.support.ant.PropertyUtils;
import org.netbeans.spi.project.ui.support.ProjectChooser;
import org.netbeans.spi.project.ui.templates.support.Templates;
import org.openide.WizardDescriptor;
import org.openide.filesystems.FileLock;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.Repository;
import org.openide.loaders.DataFolder;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectNotFoundException;
import org.openide.util.Exceptions;
import org.openide.util.NbBundle;
import org.openide.util.Utilities;

public class SEPluginProjectWizardIterator implements WizardDescriptor.InstantiatingIterator {

    public final static String PROJECT_DIR = "projdir";
    public final static String PROJECT_NAME = "name";
    public final static String PACKAGE_NAME = "package";
    public final static String CAMEL_HOME = "camelhome";
    public final static String DEF_PROJECT_NAME_VALUE = "CamelJBIModule";
    private int index;
    private WizardDescriptor.Panel[] panels;
    private WizardDescriptor wiz;

    public SEPluginProjectWizardIterator() {
    }

    public static SEPluginProjectWizardIterator createIterator() {
        return new SEPluginProjectWizardIterator();
    }

    private WizardDescriptor.Panel[] createPanels() {
        return new WizardDescriptor.Panel[]{
                    new SEPluginProjectWizardPanel()
                };
    }

    private String[] createSteps() {
        return new String[]{
                    NbBundle.getMessage(SEPluginProjectWizardIterator.class, "LBL_CreateProjectStep")
                };
    }

    public Set<Object> instantiate(/*ProgressHandle handle*/) throws IOException {

        Set<Object> resultSet = createSEDeployPluginProject();
        // save the current projects folder
        File dirF = FileUtil.normalizeFile((File) wiz.getProperty(PROJECT_DIR));
        File parent = dirF.getParentFile();
        if (parent != null && parent.exists()) {
            ProjectChooser.setProjectsFolder(parent);
        }
        return resultSet;
    }

    public void initialize(WizardDescriptor wiz) {
        this.wiz = wiz;
        String projectName = DEF_PROJECT_NAME_VALUE;
        String packageName = "";
        try {
            FileObject templateFO = Templates.getTemplate(wiz);
            DataObject templateDO = DataObject.find(templateFO);
            projectName = templateDO.getName();
            projectName = generateProjectName(ProjectChooser.getProjectsFolder(), projectName);
            packageName = generatePackageName(projectName);
        // this.wiz.putProperty(PROJECT_NAME, templateDO.getName());
        } catch (Exception ex) {
            // this.wiz.putProperty(PROJECT_NAME, DEF_PROJECT_NAME_VALUE);
        }

        this.wiz.putProperty(PROJECT_NAME, projectName);
        this.wiz.putProperty(PACKAGE_NAME, packageName);


        index = 0;
        panels = createPanels();
        // Make sure list of steps is accurate.
        String[] steps = createSteps();
        for (int i = 0; i < panels.length; i++) {
            Component c = panels[i].getComponent();
            if (steps[i] == null) {
                // Default step name to component name of panel.
                // Mainly useful for getting the name of the target
                // chooser to appear in the list of steps.
                steps[i] = c.getName();
            }
            if (c instanceof JComponent) { // assume Swing components

                JComponent jc = (JComponent) c;
                // Step #.
                jc.putClientProperty("WizardPanel_contentSelectedIndex", new Integer(i));
                // Step name (actually the whole list for reference).
                jc.putClientProperty("WizardPanel_contentData", steps);
            }
        }
    }

    public void uninitialize(WizardDescriptor wiz) {
        this.wiz.putProperty(PROJECT_DIR, null);
        this.wiz.putProperty(PROJECT_NAME, null);
        this.wiz = null;
        panels = null;
    }

    public String name() {
        return MessageFormat.format("{0} of {1}",
                new Object[]{new Integer(index + 1), new Integer(panels.length)});
    }

    public boolean hasNext() {
        return index < panels.length - 1;
    }

    public boolean hasPrevious() {
        return index > 0;
    }

    public void nextPanel() {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        index++;
    }

    public void previousPanel() {
        if (!hasPrevious()) {
            throw new NoSuchElementException();
        }
        index--;
    }

    public WizardDescriptor.Panel current() {
        return panels[index];
    }

    // If nothing unusual changes in the middle of the wizard, simply:
    public final void addChangeListener(ChangeListener l) {
    }

    public final void removeChangeListener(ChangeListener l) {
    }

    protected Set<Object> createSEDeployPluginProject() throws IOException {

        Set<Object> resultSet = new LinkedHashSet<Object>();

        File prjDirFile = FileUtil.normalizeFile((File) wiz.getProperty(PROJECT_DIR));
        String prjName = (String) wiz.getProperty(PROJECT_NAME);

        SEPluginProjectGenerator prjGenerator = new SEPluginProjectGenerator();
        prjGenerator.setCamelHome((String)wiz.getProperty(CAMEL_HOME));
        AntProjectHelper h = prjGenerator.createProject(prjDirFile, prjName);

        FileObject projectDirFO = h.getProjectDirectory();
        resultSet.add(projectDirFO);
        
        Set<Object> defArtifacts = createProjectSpecificArtifacts(projectDirFO);
        resultSet.addAll(defArtifacts);

        return resultSet;
    }

    private FileObject copyResource(FileObject dirFO, String name, String resourcePath) throws IOException {
        FileObject dataFO = null;
        FileLock outLock = null;
        OutputStream outS = null;
        InputStream inS = null;

        try {
            inS = this.getClass().getResourceAsStream(resourcePath);
            dataFO = FileUtil.createData(dirFO, name);
            outLock = dataFO.lock();
            outS = dataFO.getOutputStream(outLock);
            FileUtil.copy(inS, outS);
        } catch (Exception ex) {
            ex.printStackTrace();
        } finally {
            if (inS != null) {
                try {
                    inS.close();
                } catch (Exception ex) {
                    //ingore
                }
            }
            if (outS != null) {
                try {
                    outS.close();
                } catch (Exception ex) {
                    //ingore
                }
            }
            if (outLock != null) {
                outLock.releaseLock();
            }
        }
        return dataFO;
    }

    private FileObject createFromTemplate(FileObject dirFO, String templatePath) {
        return createFromTemplate(dirFO, templatePath, new HashMap<String, String>());
    }
    
    private FileObject createFromTemplate(FileObject dirFO, String templatePath, Map<String, String> params ) {
        
        try {
            FileObject templateFO = Repository.getDefault().getDefaultFileSystem().findResource(templatePath); // NOI18N
            DataFolder dataFolder = DataFolder.findFolder(dirFO);

            DataObject tempDO = DataObject.find(templateFO);
            
            Map<String, String> allParams = new HashMap<String, String>();
            
            if ( params != null ) {
              allParams.putAll(params);
            }
            String packageName = (String) wiz.getProperty(PACKAGE_NAME);
            allParams.put(PACKAGE_NAME, packageName);
            DataObject dObj = tempDO.createFromTemplate(dataFolder, null, allParams);
            return dObj.getPrimaryFile();
        } catch (IOException ex) {
            ex.printStackTrace();
        } 
        return null;
    }

    protected FileObject createSUConfigFile(FileObject prjDirFO) {
        FileObject nbPrjDirFO = prjDirFO.getFileObject("nbproject");
        FileObject suConfigFO = null;
        if ( nbPrjDirFO != null ) {
            suConfigFO = createFromTemplate(nbPrjDirFO, "/camel-jbi-se/su-config.xml");
        } else {
            // log.
        }
        return suConfigFO;
    }
    
    protected Set<Object> createProjectSpecificArtifacts(FileObject projectDirFO) throws IOException {
        Set<Object> resultSet = new LinkedHashSet<Object>();
        //TODO: create any default service unit artifacts needed.        
        try {

            FileObject srcDirFO = projectDirFO.getFileObject(SEPluginProjectProperties.SRC_DIR_VALUE);
            resultSet.add(srcDirFO);
            
            FileObject testDataDirFO = FileUtil.createFolder(projectDirFO, "test/data");

            // create service unit jbi descriptor
            FileObject metaInfDirFO = FileUtil.createFolder(srcDirFO, "META-INF");
            FileObject springConfigDirFO = FileUtil.createFolder(metaInfDirFO, "spring");
            String packageName = (String) wiz.getProperty(PACKAGE_NAME);
            String packagePath = packageName.replace(".", "/");
            FileObject packageDirFO = FileUtil.createFolder(srcDirFO, packagePath);

            // TODO overwrite the default jbi.xml file created in based class if necessary
            Map<String, String> params = new HashMap<String,String>();
            String suName = (String) wiz.getProperty(PROJECT_NAME);
            if ( suName == null || suName.trim().length() == 0) {
                suName = "jbi2camel-app";
            }
            suName = PropertyUtils.getUsablePropertyName(suName);
            params.put("su_name", suName);
            
            String prjDirPath = FileUtil.toFile(projectDirFO).toURL().getFile();
            if ( prjDirPath.endsWith("/")) {
                prjDirPath = prjDirPath.substring(0, prjDirPath.length()-1);
            }
            params.put("su_prj_dir", prjDirPath);
            
            String bldDirPath = prjDirPath + "/" + SEPluginProjectProperties.BUILD_DIR_VALUE;
            params.put("su_build_dir", bldDirPath);
            
            FileObject camelConfigFO = createFromTemplate(springConfigDirFO, "/Templates/Camel/camel-context.xml", params);
            resultSet.add(camelConfigFO);
            
            FileObject xsdFO = createFromTemplate(srcDirFO, "/Templates/Camel/jbi2camel.xsd", params);
            resultSet.add(xsdFO);
            
            FileObject wsdlFO = createFromTemplate(srcDirFO, "/Templates/Camel/jbi2camel.wsdl", params);
            resultSet.add(wsdlFO);
            
            FileObject jbiXmlFO = createFromTemplate(metaInfDirFO, "/Templates/Camel/jbi.xml", params);
            resultSet.add(jbiXmlFO);           
            
            FileObject routeBuilderJavaFO = createFromTemplate(packageDirFO, "/Templates/Camel/AppRouteBuilder.java", params);
            resultSet.add(routeBuilderJavaFO);            
            
            createFromTemplate(testDataDirFO, "/camel-jbi-se/testdata/message1.xml");
            createFromTemplate(testDataDirFO, "/camel-jbi-se/testdata//message2.xml");
            // createFromTemplate(srcDirFO, "/camel-jbi-se/log4j.properties");
            createSUConfigFile(projectDirFO);
            
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return resultSet;
    }

    public static boolean isValidPackageName(String pkgName) {
        String trimedPkgName = pkgName.trim();
        if (trimedPkgName.length() == 0) {
            return false; // don't allow root package eventhough it is valid

        }
        StringTokenizer tk = new StringTokenizer(trimedPkgName, "."); //NOI18N

        boolean valid = true;
        while (tk.hasMoreTokens()) {
            String token = tk.nextToken();
            if (token.length() == 0 || !Utilities.isJavaIdentifier(token)) {
                valid = false;
                break;
            }
        }
        return valid;
    }

    public static String generateProjectName(File prjLoc, String defName) {
        String prjName = defName;
        for (int i = 1; i < 100; ++i) {
            File prjFolder = new File(prjLoc, prjName);
            if (prjFolder.exists()) {
                prjName = defName + i;
                continue;
            } else {
                break;
            }
        }
        return prjName;
    }

    public static String generatePackageName(String displayName) {

        StringBuffer builder = new StringBuffer();
        boolean firstLetter = true;
        for (int i = 0; i < displayName.length(); i++) {
            char c = displayName.charAt(i);
            if ((!firstLetter && Character.isJavaIdentifierPart(c)) || (firstLetter && Character.isJavaIdentifierStart(c))) {
                firstLetter = false;
                if (Character.isUpperCase(c)) {
                    c = Character.toLowerCase(c);
                }
                builder.append(c);
            }
        }
        String packageName = builder.toString();
        return packageName;
    }
}
