/*
 * SEPluginProjectCustomizerModel.java
 *
 */

package org.openesb.components.camelse.nb.plugin.project.customizer;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import javax.swing.ButtonModel;
import javax.swing.text.Document;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.spi.project.support.ant.PropertyUtils;
import org.openesb.components.camelse.nb.plugin.project.SEPluginProjectProperties;
import org.netbeans.spi.project.support.ant.AntProjectHelper;
import org.netbeans.spi.project.support.ant.EditableProperties;
import org.netbeans.spi.project.support.ant.PropertyEvaluator;
import org.netbeans.spi.project.support.ant.ReferenceHelper;
import org.netbeans.spi.project.support.ant.ui.StoreGroup;
import org.openide.ErrorManager;
import org.openide.filesystems.FileObject;
import org.openide.util.Mutex;
import org.openide.util.MutexException;

/**
 *
 * @author chikkala
 */
public class SEPluginProjectCustomizerModel {

    private Project mProject;
    private AntProjectHelper mAntPrjHelper;   
    private ReferenceHelper mRefHelper;
    
    private StoreGroup mPrjPropsStore;
    
    private Document mSUTargetModel;
    private Document mSUNameModel;
    private Document mSUDescModel;
    
    private Document mSUZipModel;
    private ButtonModel mSUZipCompressModel;
    private Document mBuildFilesExcludesModel;
    
    private Document mCamelHomeModel;
    private CamelClassPathModel mCamelCPModel;
    
    /** Creates a new instance of Customizer UI Model and initializes it */
    public SEPluginProjectCustomizerModel(Project project, AntProjectHelper antProjectHelper, ReferenceHelper refHelper) {
        this.mProject = project;
        this.mAntPrjHelper = antProjectHelper;
        this.mRefHelper = refHelper;        
        this.mPrjPropsStore = new StoreGroup();
        init();
    }
    public Document getSUTargetModel() {
        return this.mSUTargetModel;
    }
    public Document getSUNameModel() {
        return this.mSUNameModel;
    }
    public Document getSUDescriptionModel() {
        return this.mSUDescModel;
    }    
    
    public Document getSUZipModel() {
        return this.mSUZipModel;
    }

    public ButtonModel getJarCompressModel() {
        return this.mSUZipCompressModel;
    }
    
    public Document getBuildFilesExcludesModel() {
        return this.mBuildFilesExcludesModel;
    }
    
    public Document getCamelHomeModel() {
        return this.mCamelHomeModel;
    }    
    
    public CamelClassPathModel getCamelClassPathModel() {
        return this.mCamelCPModel;
    }
    
    /** Initializes the visual models 
     */    
    private void init() {
        // initialize visual models from project properties        
        PropertyEvaluator evaluator = this.mAntPrjHelper.getStandardPropertyEvaluator();
        // cutomizer-general
        this.mSUTargetModel = this.mPrjPropsStore.createStringDocument(evaluator, SEPluginProjectProperties.JBI_SU_TARGET_NAME);
        this.mSUNameModel = this.mPrjPropsStore.createStringDocument(evaluator, SEPluginProjectProperties.JBI_SU_NAME);
        this.mSUDescModel = this.mPrjPropsStore.createStringDocument(evaluator, SEPluginProjectProperties.JBI_SU_DESCRIPTION);
        // customizer-package
        this.mSUZipModel = this.mPrjPropsStore.createStringDocument(evaluator, SEPluginProjectProperties.JBI_SU_ZIP);
        this.mSUZipCompressModel = this.mPrjPropsStore.createToggleButtonModel( evaluator, SEPluginProjectProperties.JAR_COMPRESS );
        this.mBuildFilesExcludesModel = this.mPrjPropsStore.createStringDocument(evaluator, SEPluginProjectProperties.BUILD_FILES_EXCLUDES);
        // customizer-camel
        this.mCamelHomeModel = this.mPrjPropsStore.createStringDocument(evaluator, SEPluginProjectProperties.CAMEL_HOME);
        initCamelClassPathModel();
    }
    /** Save visual models to project properties and other metadata
     */    
    public void save() {
        
        try {                        
            // Store properties 
            @SuppressWarnings("unchecked")
            Boolean result = (Boolean) ProjectManager.mutex().writeAccess(new Mutex.ExceptionAction() {
                final FileObject projectDir = mAntPrjHelper.getProjectDirectory();
                public Object run() throws IOException {
                    //TODO: regenreate any project build script and project metadata if required.
                    // store project properties.
                    storeProperties();
                    return Boolean.TRUE;
                }
            });
            // and save project if required.
            if (result == Boolean.TRUE) {
                System.err.println("#### Saving the project");
                ProjectManager.getDefault().saveProject(mProject);
            }
        } 
        catch (MutexException e) {
            ErrorManager.getDefault().notify((IOException)e.getException());
        }
        catch ( IOException ex ) {
            ErrorManager.getDefault().notify( ex );
        }
        
    }
    
    private void storeProperties() throws IOException {
        EditableProperties projectProperties = mAntPrjHelper.getProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH);
        this.mPrjPropsStore.store(projectProperties);
        // update camel classpath
        saveCamelClassPathModel(projectProperties);
        mAntPrjHelper.putProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH, projectProperties);
    }
    
    private List<String> getCamelClassPathElements() {
        EditableProperties projectProperties = mAntPrjHelper.getProperties(AntProjectHelper.PROJECT_PROPERTIES_PATH);
        String camelClassPath = projectProperties.getProperty(SEPluginProjectProperties.LIBS_CAMEL_CLASSPATH);
        String[] tokens = PropertyUtils.tokenizePath(camelClassPath);
        String camelHomeVar = "${" + SEPluginProjectProperties.CAMEL_HOME + "}/";
        List<String> relPaths = new ArrayList<String>();
        for ( String token : tokens ) {
            String relPath = null;
            if ( token.startsWith(camelHomeVar)) {
                relPath = token.substring(camelHomeVar.length());
            } else {
                relPath = token;
            }
            relPaths.add(relPath);            
        }
        return relPaths;
        
    }
    private void initCamelClassPathModel() {
        this.mCamelCPModel = new CamelClassPathModel();
        this.mCamelCPModel.load(getCamelClassPathElements());
    }
    
    private void saveCamelClassPathModel(EditableProperties prjProps) {
        List<String> relPaths = this.mCamelCPModel.getRelativePaths();
        String camelHomeVar = "${" + SEPluginProjectProperties.CAMEL_HOME + "}/";
        List<String> classPaths = new ArrayList<String>();
        for ( int i = 0;  i < relPaths.size(); ++i ) {
            String relPath = relPaths.get(i);
            if ( i < relPaths.size()-1) {
                classPaths.add(camelHomeVar + relPath + ":");
            } else {
                classPaths.add(camelHomeVar + relPath);
            }
        }
        prjProps.setProperty(SEPluginProjectProperties.LIBS_CAMEL_CLASSPATH, classPaths.toArray(new String[0]));
    }
}
