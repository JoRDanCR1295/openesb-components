/*
 * Stats.java
 *
 * Created on March 28, 2007, 1:00 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.soabi.proceng1ejb;

/**
 *
 * @author fkieviet
 */
public class Stats {
    private long last;
    private int invokes;
    private int items;
    
    /** Creates a new instance of Stats */
    public Stats() {
    }
    
    public long getLast() {
        return last;
    }
    
    public int getInvokes() {
        return invokes;
    }
    
    public int getItems() {
        return items;
    }
    
    public void setItems(int items) {
        this.items = items;
    }
    
    public void setInvokes(int invokes) {
        this.invokes = invokes;
    }
    
    public void setLast(long last) {
        this.last = last;
    }
    
    public synchronized void update(int invokes, int items) {
        this.invokes += invokes;
        this.items += items;
        this.last = System.nanoTime();
    }
    
    public synchronized Stats copy() {
        Stats ret = new Stats();
        ret.last = last;
        ret.invokes = invokes;
        ret.items = items;
        return ret;
    }
}
