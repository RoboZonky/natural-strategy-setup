package cz.janhrcek.nss;

class ProgressBar {

    void update(int done, int total) {
        StringBuilder progress = new StringBuilder(60);
        int percent = done * 100 / total;
        for (int i = 0; i < percent / 2; i++) {
            progress.append('=');
        }

        System.out.printf("\r%3d%% [%-50s]", percent, progress);
    }
}