package cz.janhrcek.nss;

class ProgressBar {

    void update(int done, int total) {
        StringBuilder bar = new StringBuilder(60);
        int percent = done * 100 / total;
        for (int i = 0; i < percent / 2; i++) {
            bar.append('=');
        }

        System.out.printf("\r%d/%d (%d%%) [%-50s]", done, total, percent, bar);
    }
}