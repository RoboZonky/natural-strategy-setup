package cz.janhrcek.nss;

class ProgressBar {

    void update(int done, int total) {
        int percent = done * 100 / total;
        String bar = "=".repeat(percent/2);
        System.out.printf("\r%d/%d (%d%%) [%-50s]", done, total, percent, bar);
    }
}