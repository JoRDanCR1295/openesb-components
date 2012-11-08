/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.tester;

import java.io.File;
import java.io.FileFilter;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

/**
 *
 * @author radval
 */
public class TestTrigger {

    public static final String TEST_TRIGGER_FILE = "trigger.txt";
    
    private TestFolder mTestFolder;
    
    private ScheduledExecutorService mService;
    
    private boolean mStop = false;
    
    private File mTestDir;
    
    public TestTrigger(TestFolder folder, ScheduledExecutorService service) {
        this.mTestFolder = folder;
        mTestDir = this.mTestFolder.getTestDir();
        mService = service;
        
    }
    
    public void startPolling() {
    	mService.scheduleAtFixedRate(new WatchFileHandler(), 0L, 1L, TimeUnit.SECONDS);
    }
    
    class WatchFileHandler implements Runnable {

        public void run() {
                File[] files = mTestDir.listFiles(new TriggerFileFilter());
                if(files != null && files.length > 0) {
                    files[0].delete();
                    mTestFolder.execute();
                }
        }
        
        
    }
    
    class TriggerFileFilter implements FileFilter {

        public boolean accept(File pathname) {
            if(pathname.getName().equals("trigger.txt")) {
                return true;
            }
            
            return false;
        }
        
    }
}
