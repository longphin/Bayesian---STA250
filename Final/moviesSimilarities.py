#-*-coding: utf-8 -*-

__author__ = 'Marcel Caraciolo <caraciol@gmail.com>'

from mrjob.job import MRJob
from metrics import correlation
from metrics import cosine, regularized_correlation
from math import sqrt

try:
    from itertools import combinations
except ImportError:
    from metrics import combinations


PRIOR_COUNT = 10
PRIOR_CORRELATION = 0


class SemicolonValueProtocol(object):

    # don't need to implement read() since we aren't using it

    def write(self, key, values):
        return ';'.join(str(v) for v in values)


class MoviesSimilarities(MRJob):

    OUTPUT_PROTOCOL = SemicolonValueProtocol

    def steps(self):
        return [
            self.mr(mapper=self.group_by_user_rating,
                    reducer=self.count_ratings_users_freq),
            self.mr(mapper=self.pairwise_items,
                    reducer=self.calculate_similarity),
            self.mr(mapper=self.calculate_ranking,
                    reducer=self.top_similar_items)]

    def group_by_user_rating(self, key, line):
        user_id, item_id, rating = line.split('|')
        #yield (item_id, int(rating)), user_id
        #yield item_id, (user_id, int(rating))
        yield user_id, (item_id, float(rating))
        #yield (user_id, item_id), int(rating)

    def count_ratings_users_freq(self, user_id, values):
        item_count = 0
        item_sum = 0
        final = []
        for item_id, rating in values:
            item_count += 1
            item_sum += rating
            final.append((item_id, rating))

        yield user_id, (item_count, item_sum, final)

    def pairwise_items(self, user_id, values):
        item_count, item_sum, ratings = values
        #print item_count, item_sum, [r for r in combinations(ratings, 2)]
        #bottleneck at combinations
        for item1, item2 in combinations(ratings, 2):
            yield (item1[0], item2[0]), \
                    (item1[1], item2[1])

    def calculate_similarity(self, pair_key, lines):
        sum_xx, sum_xy, sum_yy, sum_x, sum_y, n = (0.0, 0.0, 0.0, 0.0, 0.0, 0)
        item_pair, co_ratings = pair_key, lines
        item_xname, item_yname = item_pair
        for item_x, item_y in lines:
            sum_xx += item_x * item_x
            sum_yy += item_y * item_y
            sum_xy += item_x * item_y
            sum_y += item_y
            sum_x += item_x
            n += 1

        corr_sim = correlation(n, sum_xy, sum_x, \
                 sum_y, sum_xx, sum_yy)

        yield (item_xname, item_yname), (corr_sim, n)

    def calculate_ranking(self, item_keys, values):
        corr_sim, n = values
        item_x, item_y = item_keys
        if int(n) > 0:
            yield (item_x, corr_sim), \
                     (item_y, n)

    def top_similar_items(self, key_sim, similar_ns):
        item_x, corr_sim, = key_sim
        for item_y, n in similar_ns:
            yield None, (item_x, item_y, corr_sim, jaccard_sim, n)


if __name__ == '__main__':
    MoviesSimilarities.run()
