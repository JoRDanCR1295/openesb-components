package test.jbi.integration.testbc.installer;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.zip.CRC32;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

final public class TestHelper {

	public static void jarAllFiles(String base, String destJarFile)
			throws IOException {
		File baseDir = new File(base);
		if (!baseDir.exists() || !baseDir.isDirectory()) {
			throw new RuntimeException("Invalid base directory");
		}
		// First get all the files under base directory
		List<String[]> list = new ArrayList<String[]>();
		recursiveTraverse(baseDir, list, "");

		jarFiles(destJarFile, list);

	}

	public static void jarFiles(String destJarFile, List<String[]> list)
			throws IOException {
		FileOutputStream fos = new FileOutputStream(destJarFile);
		ZipOutputStream zos = new ZipOutputStream(fos);
		int bytesRead;
		byte[] buffer = new byte[1024];
		CRC32 crc = new CRC32();
		for (Iterator<String[]> iter = list.iterator(); iter.hasNext();) {
			String[] fileDetails = iter.next();
			File file = new File(fileDetails[0]);
			BufferedInputStream bis = new BufferedInputStream(
					new FileInputStream(file));
			crc.reset();
			while ((bytesRead = bis.read(buffer)) != -1) {
				crc.update(buffer, 0, bytesRead);
			}
			bis.close();
			// Reset to beginning of input stream
			bis = new BufferedInputStream(new FileInputStream(file));
			ZipEntry entry = new ZipEntry((fileDetails[1].equals("") ? ""
					: fileDetails[1])
					+ file.getName());
			entry.setMethod(ZipEntry.STORED);
			entry.setCompressedSize(file.length());
			entry.setSize(file.length());
			entry.setCrc(crc.getValue());
			zos.putNextEntry(entry);
			while ((bytesRead = bis.read(buffer)) != -1) {
				zos.write(buffer, 0, bytesRead);
			}
			bis.close();
		}
		zos.close();
	}

	public static void recursiveTraverse(File base, List<String[]> l,
			String relativePath) {
		File[] files = base.listFiles();
		for (int i = 0; i < files.length; ++i) {
			if (files[i].isDirectory()) {
				recursiveTraverse(files[i], l, relativePath
						+ files[i].getName() + "/");
			} else {
				l
						.add(new String[] { files[i].getAbsolutePath(),
								relativePath });
			}
		}
	}

	public static boolean deleteDirectory(File path) {
		if (path.exists()) {
			File[] files = path.listFiles();
			for (int i = 0; i < files.length; i++) {
				if (files[i].isDirectory()) {
					deleteDirectory(files[i]);
				} else {
					files[i].delete();
				}
			}
		}
		return (path.delete());
	}

	static public void copyFile(String meta, String fileName,
			String destinationName) throws IOException {
		FileOutputStream fout = new FileOutputStream(meta + File.separator
				+ destinationName);
		BufferedInputStream bis = new BufferedInputStream(
				new FileInputStream(fileName));
		int bytesRead;
		byte[] buffer = new byte[1024];
		while ((bytesRead = bis.read(buffer)) != -1) {
			fout.write(buffer, 0, bytesRead);
		}
		bis.close();
		fout.close();
	}

	static public void copyFile(String meta, String fileName) throws IOException {
		File f = new File(fileName);
		if (!f.exists()) {
			System.out.println("WARN: Could not add WSDL " + f.getName()
					+ " to SU");
			return;
		}
		copyFile(meta, fileName, f.getName());
	}
}
